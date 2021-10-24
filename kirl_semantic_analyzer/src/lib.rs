use std::borrow::Cow;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Range;

use dec::Decimal128;
use regex::Regex;
use uuid::Uuid;

use kirl_parser::kirl_parser::{AnonymousStructType, Function, FunctionType, ImportPath, KirlTopLevelStatement, NamedType, Pattern, Statement, StatementItem, Struct, StructName, Type};
use kirl_parser::CharacterPosition;

use crate::name_resolver::ResolvedItems;
use crate::syntax_tree_to_hir::SearchPaths;

pub mod name_resolver;
pub mod syntax_tree_to_hir;
pub mod type_checker;

#[derive(Debug)]
pub struct WithImport<T> {
    pub import: ImportPath,
    pub item: T,
}

#[derive(Debug)]
pub struct KirlTopLevelItems {
    pub statements: Vec<Statement>,
    pub structs: Vec<WithImport<Struct>>,
    pub functions: Vec<WithImport<Function>>,
}

pub fn collect_top_level_item_with_imports(top_level_statements: Vec<KirlTopLevelStatement>) -> KirlTopLevelItems {
    let mut statements = Vec::new();
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    let mut imports = Vec::new();
    for statement in top_level_statements {
        match statement {
            KirlTopLevelStatement::Statement((_, normal_statement)) => {
                if let StatementItem::Import(import) = &normal_statement.statement {
                    imports.push(import.clone());
                }
                statements.push(normal_statement);
            }
            KirlTopLevelStatement::FunctionDefinition((_, function_definition)) => {
                functions.push(WithImport { import: ImportPath::List(imports.clone()), item: function_definition });
            }
            KirlTopLevelStatement::StructDefinition((_, struct_definition)) => {
                structs.push(WithImport { import: ImportPath::List(imports.clone()), item: struct_definition });
            }
        }
    }
    KirlTopLevelItems { statements, structs, functions }
}

#[derive(Debug, PartialEq, Clone)]
pub struct HIRStatementList<Reference>(pub Vec<HIRStatement<Reference>>);

impl<Reference> From<Vec<HIRStatement<Reference>>> for HIRStatementList<Reference> {
    fn from(value: Vec<HIRStatement<Reference>>) -> Self {
        HIRStatementList(value)
    }
}

impl<Reference> From<HIRStatementList<Reference>> for Vec<HIRStatement<Reference>> {
    fn from(value: HIRStatementList<Reference>) -> Self {
        value.0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum HIRStatement<Reference> {
    Binding { variable_id: usize, variable_type: HIRType, expression: HIRExpression<Reference> },
    Return(Variable<Reference>),
    Continue(Option<String>),
    Break(Option<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum HIRExpression<Reference> {
    Immediate(Immediate),
    CallFunction {
        function: Variable<Reference>,
        arguments: Vec<Variable<Reference>>,
    },
    AccessVariable(Variable<Reference>),
    AccessMember {
        variable: Variable<Reference>,
        member: String,
    },
    AccessTupleItem {
        variable: Variable<Reference>,
        index: usize,
    },
    If {
        condition: Variable<Reference>,
        then: (Vec<HIRStatement<Reference>>, Variable<Reference>),
        other: (Vec<HIRStatement<Reference>>, Variable<Reference>),
    },
    IfLet {
        condition_binding: usize,
        pattern_type: HIRType,
        condition: Variable<Reference>,
        then: (Vec<HIRStatement<Reference>>, Variable<Reference>),
        other: (Vec<HIRStatement<Reference>>, Variable<Reference>),
    },
    Loop(Vec<HIRStatement<Reference>>),
    Assign {
        variable: ReferenceAccess<Reference>,
        value: Variable<Reference>,
    },
    // ConstructClosure,(TODO)
    ConstructStruct(BTreeMap<String, Variable<Reference>>),
    ConstructTuple(Vec<Variable<Reference>>),
    ConstructArray(Vec<Variable<Reference>>),
}

#[derive(Debug, PartialEq, Clone, Ord, PartialOrd, Eq)]
pub enum HIRType {
    Infer,
    Unreachable,
    Named { path: Vec<String>, generics_arguments: Vec<HIRType> },
    Tuple(Vec<HIRType>),
    Array(Box<HIRType>),
    Function { arguments: Vec<HIRType>, result: Box<HIRType> },
    AnonymousStruct(BTreeMap<String, HIRType>),
    Or(Vec<HIRType>),
}

#[derive(Debug)]
pub enum HIRTypeConvertError {
    DuplicatedMember(String),
}

impl Display for HIRTypeConvertError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HIRTypeConvertError::DuplicatedMember(name) => write!(f, "member {:?} is duplicated.", name),
        }
    }
}

impl Error for HIRTypeConvertError {}

pub(crate) trait TryMapCollect: Sized + IntoIterator {
    fn try_map_collect<T, E>(self, mut map: impl FnMut(Self::Item) -> Result<T, E>) -> Result<Vec<T>, E> {
        let mut result = Vec::new();
        for item in self {
            result.push(map(item)?);
        }
        Ok(result)
    }
}

impl<I: Sized + IntoIterator> TryMapCollect for I {}

impl TryFrom<Type> for HIRType {
    type Error = HIRTypeConvertError;
    fn try_from(ty: Type) -> Result<Self, Self::Error> {
        match ty {
            Type::None => Ok(HIRType::Tuple(Vec::new())),
            Type::Unreachable(_) => Ok(HIRType::Unreachable),
            Type::NamedType(NamedType { path, generics_arguments, .. }) => Ok(HIRType::Named {
                path,
                generics_arguments: generics_arguments.into_iter().try_map_collect(TryInto::try_into)?,
            }),
            Type::Tuple(_, items) => Ok(HIRType::Tuple(items.into_iter().try_map_collect(TryInto::try_into)?)),
            Type::Array(_, item) => Ok(HIRType::Array(Box::new((*item).try_into()?))),
            Type::Function(FunctionType { argument, result, .. }) => Ok(HIRType::Function {
                arguments: argument.into_iter().try_map_collect(TryInto::try_into)?,
                result: Box::new((*result).try_into()?),
            }),
            Type::AnonymousStruct(AnonymousStructType { members, .. }) => {
                let mut result_members = BTreeMap::new();
                for (member, ty) in members {
                    if result_members.contains_key(&member) {
                        return Err(HIRTypeConvertError::DuplicatedMember(member));
                    }
                    result_members.insert(member, ty.try_into()?);
                }
                Ok(HIRType::AnonymousStruct(result_members))
            }
            Type::Or(_, items) => Ok(HIRType::Or(items.into_iter().try_map_collect(TryInto::try_into)?)),
        }
    }
}

impl TryFrom<&Pattern> for HIRType {
    type Error = HIRTypeConvertError;
    fn try_from(value: &Pattern) -> Result<Self, Self::Error> {
        match value {
            Pattern::Variable(_) => Ok(HIRType::Infer),
            Pattern::Tuple(items) => {
                let mut result = Vec::with_capacity(items.len());
                for pat in items {
                    result.push(pat.try_into()?);
                }
                Ok(HIRType::Tuple(result))
            }
            Pattern::Struct(StructName::Named(NamedType { path, generics_arguments, .. }), _) => Ok(HIRType::Named {
                path: path.clone(),
                generics_arguments: generics_arguments.iter().cloned().try_map_collect(TryInto::try_into)?,
            }),
            Pattern::Struct(StructName::Anonymous, members) => {
                let mut result_members = BTreeMap::new();
                for (name, pattern) in members {
                    if result_members.contains_key(name) {
                        return Err(HIRTypeConvertError::DuplicatedMember(name.clone()));
                    }
                    result_members.insert(name.clone(), pattern.try_into()?);
                }
                Ok(HIRType::AnonymousStruct(result_members))
            }
        }
    }
}

impl HIRType {
    pub fn is_a(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (HIRType::Infer, _) => true,
            (_, HIRType::Infer) => true,
            (HIRType::Unreachable, _) => true,
            (_, HIRType::Unreachable) => false,
            (HIRType::Named { path: path1, generics_arguments: arg1 }, HIRType::Named { path: path2, generics_arguments: arg2 }) => path1 == path2 && arg1.len() == arg2.len() && arg1.iter().zip(arg2).all(|(ty1, ty2)| ty1.is_a(ty2)),
            (HIRType::Tuple(items1), HIRType::Tuple(items2)) => items1.len() >= items2.len() && items1.iter().zip(items2).all(|(ty1, ty2)| ty1.is_a(ty2)),
            (HIRType::Array(t1), HIRType::Array(t2)) => t1.is_a(t2),
            (HIRType::Function { arguments: arg1, result: res1 }, HIRType::Function { arguments: arg2, result: res2 }) => arg1.len() == arg2.len() && arg2.iter().zip(arg1).all(|(ty1, ty2)| ty1.is_a(ty2)) && res1.is_a(res2),
            (HIRType::AnonymousStruct(members1), HIRType::AnonymousStruct(members2)) => members2.iter().all(|(k, v2)| members1.get(k).map_or(false, |v1| v1.is_a(v2))),
            (HIRType::Or(items1), ty2) => items1.iter().all(|ty1| ty1.is_a(ty2)),
            (ty1, HIRType::Or(items2)) => items2.iter().any(|ty2| ty1.is_a(ty2)),
            _ => false,
        }
    }

    pub fn normalize(&mut self) {
        match self {
            HIRType::Tuple(items) => items.iter_mut().for_each(HIRType::normalize),
            HIRType::Array(item) => item.normalize(),
            HIRType::Function { arguments, result } => {
                arguments.iter_mut().for_each(HIRType::normalize);
                result.normalize();
            }
            HIRType::AnonymousStruct(members) => members.values_mut().for_each(HIRType::normalize),
            HIRType::Or(items) => {
                items.iter_mut().for_each(HIRType::normalize);
                *items = items.drain(..).flat_map(|ty| if let HIRType::Or(items) = ty { items } else { vec![ty] }).collect();
                items.sort();
                items.dedup();
                if items.len() > 1 {
                    items.retain(|ty| ty != &HIRType::Unreachable);
                }
                if items.len() == 1 {
                    *self = items.pop().unwrap();
                }
            }
            _ => {}
        }
    }

    pub fn member_type(&self, member_name: &str) -> Option<Cow<HIRType>> {
        match self {
            HIRType::Infer => Some(Cow::Owned(HIRType::Infer)),
            HIRType::AnonymousStruct(members) => members.get(member_name).map(Cow::Borrowed),
            HIRType::Or(items) => {
                let mut result = Vec::with_capacity(items.len());
                for ty in items.iter().map(|ty| ty.member_type(member_name)) {
                    match ty {
                        None => return None,
                        Some(ty) => result.push(ty.into_owned()),
                    }
                }
                Some(Cow::Owned(HIRType::Or(result)))
            }
            _ => None,
        }
    }

    pub fn tuple_nth_type(&self, n: usize) -> Option<Cow<HIRType>> {
        match self {
            HIRType::Infer => Some(Cow::Owned(HIRType::Infer)),
            HIRType::Tuple(items) => items.get(n).map(Cow::Borrowed),
            HIRType::Or(items) => {
                let mut result = Vec::with_capacity(items.len());
                for ty in items.iter().map(|ty| ty.tuple_nth_type(n)) {
                    match ty {
                        None => return None,
                        Some(ty) => result.push(ty.into_owned()),
                    }
                }
                Some(Cow::Owned(HIRType::Or(result)))
            }
            _ => None,
        }
    }

    pub fn has_member(&self, member_name: &str) -> bool {
        match self {
            HIRType::Infer => true,
            HIRType::AnonymousStruct(members) => members.contains_key(member_name),
            HIRType::Or(items) => items.iter().all(|ty| ty.has_member(member_name)),
            _ => false,
        }
    }

    pub fn has_tuple_nth(&self, n: usize) -> bool {
        match self {
            HIRType::Infer => true,
            HIRType::Tuple(items) => n < items.len(),
            HIRType::Or(items) => items.iter().all(|ty| ty.has_tuple_nth(n)),
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ReferenceAccess<Reference> {
    Variable(Variable<Reference>),
    TupleItem(Variable<Reference>, usize),
    Member(Variable<Reference>, String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Immediate {
    Number(Decimal128),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Variable<Reference> {
    Named(Range<CharacterPosition>, Reference),
    Unnamed(usize),
}

impl ToString for Variable<SearchPaths> {
    fn to_string(&self) -> String {
        match self {
            Variable::Named(_, SearchPaths(path)) if path.len() == 1 => path[0].join("::"),
            Variable::Named(_, SearchPaths(paths)) => format!("$either({})", paths.iter().map(|path| path.join("::")).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default()),
            Variable::Unnamed(id) => format!("${}", id),
        }
    }
}

impl ToString for Variable<ResolvedItems> {
    fn to_string(&self) -> String {
        match self {
            Variable::Named(_, ResolvedItems(_, candidate)) if candidate.len() == 1 => format!("{}: {}", candidate[0].1, candidate[0].1.to_string()),
            Variable::Named(_, ResolvedItems(_, candidates)) => format!("$either({})", candidates.iter().map(|(_, id, ty)| format!("{}: {}", id, ty.to_string())).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default()),
            Variable::Unnamed(id) => format!("${}", id),
        }
    }
}

impl ToString for Variable<(Uuid, HIRType)> {
    fn to_string(&self) -> String {
        match self {
            Variable::Named(_, (id, ty)) => format!("{}: {}", id, ty.to_string()),
            Variable::Unnamed(id) => format!("${}", id),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum StatementReachable {
    Reachable,
    UnreachableByReturn,
    UnreachableByBreak(Option<String>),
}

impl StatementReachable {
    fn combine(&mut self, other: Self) {
        match (self, other) {
            (this @ StatementReachable::Reachable, _) => *this = StatementReachable::Reachable,
            (this, StatementReachable::Reachable) => *this = StatementReachable::Reachable,
            (this @ StatementReachable::UnreachableByBreak(_), _) => *this = StatementReachable::UnreachableByBreak(None),
            (this, StatementReachable::UnreachableByBreak(_)) => *this = StatementReachable::UnreachableByBreak(None),
            (this, _) => *this = StatementReachable::UnreachableByReturn,
        }
    }
}

impl ToString for HIRType {
    fn to_string(&self) -> String {
        match self {
            HIRType::Infer => "$Infer".to_string(),
            HIRType::Unreachable => "!".to_string(),
            HIRType::Named { path, generics_arguments } => {
                let generics_arguments = generics_arguments.iter().map(ToString::to_string).reduce(|a, b| format!("{}, {}", a, b)).map(|a| format!("::<{}>", a)).unwrap_or_default();
                format!("{}{}", path.join("::"), generics_arguments)
            }
            HIRType::Tuple(items) => {
                let items = items.iter().map(ToString::to_string).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("({})", items)
            }
            HIRType::Array(item) => {
                format!("[{}]", item.to_string())
            }
            HIRType::Function { arguments, result } => {
                let argument = arguments.iter().map(ToString::to_string).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("({})->{}", argument, result.to_string())
            }
            HIRType::AnonymousStruct(members) => {
                let members = members.iter().map(|(name, ty)| format!("{}: {}", name, ToString::to_string(ty))).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("#{{{}}}", members)
            }
            HIRType::Or(items) => {
                let items = items.iter().map(ToString::to_string).reduce(|a, b| format!("{} | {}", a, b)).unwrap_or_default();
                format!("({})", items)
            }
        }
    }
}

fn get_ordinal(index: usize) -> &'static str {
    match index % 10 {
        _ if index / 10 == 1 => "th",
        1 => "st",
        2 => "nd",
        3 => "rd",
        _ => "th",
    }
}

impl<T> ToString for HIRExpression<T>
where
    Variable<T>: ToString,
{
    fn to_string(&self) -> String {
        match self {
            HIRExpression::Immediate(value) => match value {
                Immediate::Number(value) => format!("{}", value),
                Immediate::String(value) => format!("{:?}", value),
            },
            HIRExpression::CallFunction { function, arguments } => {
                let function = ToString::to_string(function);
                let arguments = arguments.iter().map(ToString::to_string).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("{}({})", function, arguments)
            }
            HIRExpression::AccessVariable(variable) => ToString::to_string(variable),
            HIRExpression::AccessTupleItem { variable, index } => {
                let index = *index;
                let ordinal = get_ordinal(index);
                format!("{}.{}{}", ToString::to_string(variable), index, ordinal)
            }
            HIRExpression::AccessMember { variable, member } => {
                format!("{}.{}", ToString::to_string(variable), member)
            }
            HIRExpression::If { condition, then, other } => {
                let regex = Regex::new("(^|\n)(.)").unwrap();
                let then_statements = then.0.iter().map(ToString::to_string).map(|stmt| format!("{}\n", stmt)).reduce(|a, b| format!("{}{}", a, b)).unwrap_or_default();
                let then = format!("{}{}", then_statements, ToString::to_string(&then.1));
                let mut result = format!("if {} {{\n{}\n}}", ToString::to_string(condition), regex.replace_all(&then, "$1\t$2"));
                let other_statements = other.0.iter().map(ToString::to_string).map(|stmt| format!("{}\n", stmt)).reduce(|a, b| format!("{}{}", a, b)).unwrap_or_default();
                let other = format!("{}{}", other_statements, ToString::to_string(&other.1));
                result.push_str(&format!(" else {{\n{}\n}}", regex.replace_all(&other, "$1\t$2")));
                result
            }
            HIRExpression::IfLet { condition_binding, pattern_type, condition, then, other } => {
                let regex = Regex::new("(^|\n)(.)").unwrap();
                let then_statements = then.0.iter().map(ToString::to_string).map(|stmt| format!("{}\n", stmt)).reduce(|a, b| format!("{}{}", a, b)).unwrap_or_default();
                let then = format!("{}{}", then_statements, ToString::to_string(&then.1));
                let mut result = format!("if let ${}: {} = {} {{\n{}\n}}", condition_binding, ToString::to_string(pattern_type), ToString::to_string(condition), regex.replace_all(&then, "$1\t$2"));
                let other_statements = other.0.iter().map(ToString::to_string).map(|stmt| format!("{}\n", stmt)).reduce(|a, b| format!("{}{}", a, b)).unwrap_or_default();
                let other = format!("{}{}", other_statements, ToString::to_string(&other.1));
                result.push_str(&format!(" else {{\n{}\n}}", regex.replace_all(&other, "$1\t$2")));
                result
            }
            HIRExpression::Loop(statements) => {
                let regex = Regex::new("(^|\n)(.)").unwrap();
                let statements = statements.iter().map(ToString::to_string).map(|stmt| format!("{}\n", stmt)).reduce(|a, b| format!("{}{}", a, b)).unwrap_or_default();
                format!("loop {{\n{}}}", regex.replace_all(&statements, "$1\t$2"))
            }
            HIRExpression::Assign { variable, value } => match variable {
                ReferenceAccess::Variable(variable) => format!("{} = {}", ToString::to_string(variable), ToString::to_string(value)),
                ReferenceAccess::TupleItem(variable, index) => format!("{}.{}{} = {}", ToString::to_string(variable), index, get_ordinal(*index), ToString::to_string(value)),
                ReferenceAccess::Member(variable, member) => format!("{}.{} = {}", ToString::to_string(variable), member, ToString::to_string(value)),
            },
            HIRExpression::ConstructStruct(members) => {
                let members = members.iter().map(|(name, value)| format!("{}: {}", name, ToString::to_string(value))).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("#{{{}}}", members)
            }
            HIRExpression::ConstructTuple(items) => {
                let items = items.iter().map(ToString::to_string).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("({})", items)
            }
            HIRExpression::ConstructArray(items) => {
                let items = items.iter().map(ToString::to_string).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default();
                format!("[{}]", items)
            }
        }
    }
}

impl<T> ToString for HIRStatement<T>
where
    Variable<T>: ToString,
{
    fn to_string(&self) -> String {
        match self {
            HIRStatement::Binding { variable_id, variable_type, expression } => {
                format!("let ${}: {} = {};", variable_id, ToString::to_string(variable_type), ToString::to_string(expression))
            }
            HIRStatement::Return(variable) => {
                format!("return {};", ToString::to_string(variable))
            }
            HIRStatement::Continue(label) => {
                if let Some(label) = label {
                    format!("continue {};", label)
                } else {
                    "continue;".to_string()
                }
            }
            HIRStatement::Break(label) => {
                if let Some(label) = label {
                    format!("break {};", label)
                } else {
                    "break;".to_string()
                }
            }
        }
    }
}

pub fn statements_to_string<T>(statements: &[HIRStatement<T>]) -> String
where
    HIRStatement<T>: ToString,
{
    statements.iter().map(ToString::to_string).reduce(|a, b| format!("{}\n{}", a, b)).unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use kirl_parser::{CharacterPosition, KirlParser};

    use crate::syntax_tree_to_hir::{analysis_statements, SearchPaths};
    use crate::{collect_top_level_item_with_imports, statements_to_string, HIRExpression, HIRStatement, HIRType, Immediate, KirlTopLevelItems, ReferenceAccess, Variable};

    #[test]
    fn test_analysis_statements() {
        const CODE1: &str = r#"
            // TODO: test
            import std::{io::println, array};

            var number: Number = 10;
            var map: [bool] = array::fill(false, number + 1);
            var i = 4;
            while i < number + 1{
                map[i] = true;
                i = i + 2;
            }

            var i = 3;
            while i <= number {
                if map[i] { continue; };
                {
                    var j = i * i;
                    while j <= number {
                        map[j] = true;
                        j = j + i;
                    }
                };
                i = i + 2;
            }

            if !map[number] {
                println("prime");
            } else {
                println("not prime");
            };

            "#;
        const CODE2: &str = r#"
            import std::{io::println, collections::deque};

            var #{nodes} = graph::get();
            var q = deque::new();
            q.deque::push_back(0);
            while var #{value: node_index} = q.deque::pop_front() {
                println(nodes[node_index]);
                for child in nodes[node_index].graph::children() {
                    q.deque::push_back(child);
                }
            }
        "#;
        let parser = KirlParser::new();
        let statements = parser.parse(CODE1).expect("parse error");
        let KirlTopLevelItems { statements, .. } = collect_top_level_item_with_imports(statements);
        let statements = analysis_statements(statements).unwrap();
        assert_eq!(
            statements,
            vec![
                HIRStatement::Binding {
                    variable_id: 0,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::Immediate(Immediate::Number(10.into())),
                },
                HIRStatement::Binding {
                    variable_id: 1,
                    variable_type: HIRType::Named { path: vec!["Number".to_string()], generics_arguments: vec![] },
                    expression: HIRExpression::AccessVariable(Variable::Unnamed(0)),
                },
                HIRStatement::Binding {
                    variable_id: 2,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::Immediate(Immediate::Number(1.into())),
                },
                HIRStatement::Binding {
                    variable_id: 3,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(CharacterPosition { line: 5, column: 55 }..CharacterPosition { line: 5, column: 58 }, SearchPaths(vec![vec!["_add".to_string()]])),
                        arguments: vec![Variable::Unnamed(1), Variable::Unnamed(2)],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 4,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(CharacterPosition { line: 5, column: 30 }..CharacterPosition { line: 5, column: 41 }, SearchPaths(vec![vec!["array".to_string(), "fill".to_string()], vec!["std".to_string(), "array".to_string(), "fill".to_string()]])),
                        arguments: vec![Variable::Named(CharacterPosition { line: 5, column: 42 }..CharacterPosition { line: 5, column: 47 }, SearchPaths(vec![vec!["false".to_string()]])), Variable::Unnamed(3)],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 5,
                    variable_type: HIRType::Array(Box::new(HIRType::Named { path: vec!["bool".to_string()], generics_arguments: vec![] })),
                    expression: HIRExpression::AccessVariable(Variable::Unnamed(4)),
                },
                HIRStatement::Binding {
                    variable_id: 6,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::Immediate(Immediate::Number(4.into())),
                },
                HIRStatement::Binding {
                    variable_id: 7,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::AccessVariable(Variable::Unnamed(6)),
                },
                HIRStatement::Binding {
                    variable_id: 18,
                    variable_type: HIRType::Tuple(vec![]),
                    expression: HIRExpression::Loop(vec![
                        HIRStatement::Binding {
                            variable_id: 8,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::Immediate(Immediate::Number(1.into())),
                        },
                        HIRStatement::Binding {
                            variable_id: 9,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 7, column: 28 }..CharacterPosition { line: 7, column: 31 }, SearchPaths(vec![vec!["_add".to_string()]])),
                                arguments: vec![Variable::Unnamed(1), Variable::Unnamed(8)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 10,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 7, column: 19 }..CharacterPosition { line: 7, column: 22 }, SearchPaths(vec![vec!["_gt".to_string()]])),
                                arguments: vec![Variable::Unnamed(9), Variable::Unnamed(7)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 11,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 7, column: 18 }..CharacterPosition { line: 7, column: 32 }, SearchPaths(vec![vec!["_not".to_string()]])),
                                arguments: vec![Variable::Unnamed(10)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 12,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::ConstructTuple(vec![]),
                        },
                        HIRStatement::Binding {
                            variable_id: 13,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::If {
                                condition: Variable::Unnamed(11),
                                then: (vec![HIRStatement::Break(None)], Variable::Unnamed(12)),
                                other: (vec![], Variable::Unnamed(12)),
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 14,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 8, column: 20 }..CharacterPosition { line: 8, column: 21 }, SearchPaths(vec![vec!["_set_item".to_string()]])),
                                arguments: vec![Variable::Unnamed(5), Variable::Unnamed(7), Variable::Named(CharacterPosition { line: 8, column: 25 }..CharacterPosition { line: 8, column: 29 }, SearchPaths(vec![vec!["true".to_string()]]))],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 15,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::Immediate(Immediate::Number(2.into())),
                        },
                        HIRStatement::Binding {
                            variable_id: 16,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 9, column: 21 }..CharacterPosition { line: 9, column: 24 }, SearchPaths(vec![vec!["_add".to_string()]])),
                                arguments: vec![Variable::Unnamed(7), Variable::Unnamed(15)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 17,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::Assign {
                                variable: ReferenceAccess::Variable(Variable::Unnamed(7)),
                                value: Variable::Unnamed(16),
                            },
                        },
                    ]),
                },
                HIRStatement::Binding {
                    variable_id: 19,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::Immediate(Immediate::Number(3.into())),
                },
                HIRStatement::Binding {
                    variable_id: 20,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::AccessVariable(Variable::Unnamed(19)),
                },
                HIRStatement::Binding {
                    variable_id: 45,
                    variable_type: HIRType::Tuple(vec![]),
                    expression: HIRExpression::Loop(vec![
                        HIRStatement::Binding {
                            variable_id: 21,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 13, column: 19 }..CharacterPosition { line: 13, column: 23 }, SearchPaths(vec![vec!["_gt".to_string()]])),
                                arguments: vec![Variable::Unnamed(20), Variable::Unnamed(1)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 22,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 13, column: 19 }..CharacterPosition { line: 13, column: 23 }, SearchPaths(vec![vec!["_not".to_string()]])),
                                arguments: vec![Variable::Unnamed(21)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 23,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 13, column: 18 }..CharacterPosition { line: 13, column: 29 }, SearchPaths(vec![vec!["_not".to_string()]])),
                                arguments: vec![Variable::Unnamed(22)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 24,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::ConstructTuple(vec![]),
                        },
                        HIRStatement::Binding {
                            variable_id: 25,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::If {
                                condition: Variable::Unnamed(23),
                                then: (vec![HIRStatement::Break(None)], Variable::Unnamed(24)),
                                other: (vec![], Variable::Unnamed(24)),
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 26,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 14, column: 23 }..CharacterPosition { line: 14, column: 24 }, SearchPaths(vec![vec!["_get_item".to_string()]])),
                                arguments: vec![Variable::Unnamed(5), Variable::Unnamed(20)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 29,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::If {
                                condition: Variable::Unnamed(26),
                                then: (
                                    vec![
                                        HIRStatement::Continue(None),
                                        HIRStatement::Binding {
                                            variable_id: 27,
                                            variable_type: HIRType::Tuple(vec![]),
                                            expression: HIRExpression::ConstructTuple(vec![]),
                                        },
                                    ],
                                    Variable::Unnamed(27)
                                ),
                                other: (
                                    vec![HIRStatement::Binding {
                                        variable_id: 28,
                                        variable_type: HIRType::Tuple(vec![]),
                                        expression: HIRExpression::ConstructTuple(vec![]),
                                    }],
                                    Variable::Unnamed(28)
                                ),
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 30,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 16, column: 29 }..CharacterPosition { line: 16, column: 32 }, SearchPaths(vec![vec!["_mul".to_string()]])),
                                arguments: vec![Variable::Unnamed(20), Variable::Unnamed(20)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 31,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::AccessVariable(Variable::Unnamed(30)),
                        },
                        HIRStatement::Binding {
                            variable_id: 40,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::Loop(vec![
                                HIRStatement::Binding {
                                    variable_id: 32,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 17, column: 27 }..CharacterPosition { line: 17, column: 31 }, SearchPaths(vec![vec!["_gt".to_string()]])),
                                        arguments: vec![Variable::Unnamed(31), Variable::Unnamed(1)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 33,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 17, column: 27 }..CharacterPosition { line: 17, column: 31 }, SearchPaths(vec![vec!["_not".to_string()]])),
                                        arguments: vec![Variable::Unnamed(32)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 34,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 17, column: 26 }..CharacterPosition { line: 17, column: 37 }, SearchPaths(vec![vec!["_not".to_string()]])),
                                        arguments: vec![Variable::Unnamed(33)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 35,
                                    variable_type: HIRType::Tuple(vec![]),
                                    expression: HIRExpression::ConstructTuple(vec![]),
                                },
                                HIRStatement::Binding {
                                    variable_id: 36,
                                    variable_type: HIRType::Tuple(vec![]),
                                    expression: HIRExpression::If {
                                        condition: Variable::Unnamed(34),
                                        then: (vec![HIRStatement::Break(None)], Variable::Unnamed(35)),
                                        other: (vec![], Variable::Unnamed(35)),
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 37,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 18, column: 28 }..CharacterPosition { line: 18, column: 29 }, SearchPaths(vec![vec!["_set_item".to_string()]])),
                                        arguments: vec![Variable::Unnamed(5), Variable::Unnamed(31), Variable::Named(CharacterPosition { line: 18, column: 33 }..CharacterPosition { line: 18, column: 37 }, SearchPaths(vec![vec!["true".to_string()]]))],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 38,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 19, column: 29 }..CharacterPosition { line: 19, column: 32 }, SearchPaths(vec![vec!["_add".to_string()]])),
                                        arguments: vec![Variable::Unnamed(31), Variable::Unnamed(20)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 39,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::Assign {
                                        variable: ReferenceAccess::Variable(Variable::Unnamed(31)),
                                        value: Variable::Unnamed(38),
                                    },
                                },
                            ]),
                        },
                        HIRStatement::Binding {
                            variable_id: 41,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::ConstructTuple(vec![]),
                        },
                        HIRStatement::Binding {
                            variable_id: 42,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::Immediate(Immediate::Number(2.into())),
                        },
                        HIRStatement::Binding {
                            variable_id: 43,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 22, column: 21 }..CharacterPosition { line: 22, column: 24 }, SearchPaths(vec![vec!["_add".to_string()]])),
                                arguments: vec![Variable::Unnamed(20), Variable::Unnamed(42)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 44,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::Assign {
                                variable: ReferenceAccess::Variable(Variable::Unnamed(20)),
                                value: Variable::Unnamed(43),
                            },
                        },
                    ]),
                },
                HIRStatement::Binding {
                    variable_id: 46,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(CharacterPosition { line: 25, column: 20 }..CharacterPosition { line: 25, column: 26 }, SearchPaths(vec![vec!["_get_item".to_string()]])),
                        arguments: vec![Variable::Unnamed(5), Variable::Unnamed(1)],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 47,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(CharacterPosition { line: 25, column: 16 }..CharacterPosition { line: 25, column: 27 }, SearchPaths(vec![vec!["_not".to_string()]])),
                        arguments: vec![Variable::Unnamed(46)],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 54,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::If {
                        condition: Variable::Unnamed(47),
                        then: (
                            vec![
                                HIRStatement::Binding {
                                    variable_id: 48,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::Immediate(Immediate::String("prime".to_string())),
                                },
                                HIRStatement::Binding {
                                    variable_id: 49,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 26, column: 16 }..CharacterPosition { line: 26, column: 23 }, SearchPaths(vec![vec!["println".to_string()], vec!["std".to_string(), "io".to_string(), "println".to_string()]])),
                                        arguments: vec![Variable::Unnamed(48)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 50,
                                    variable_type: HIRType::Tuple(vec![]),
                                    expression: HIRExpression::ConstructTuple(vec![]),
                                },
                            ],
                            Variable::Unnamed(50)
                        ),
                        other: (
                            vec![
                                HIRStatement::Binding {
                                    variable_id: 51,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::Immediate(Immediate::String("not prime".to_string())),
                                },
                                HIRStatement::Binding {
                                    variable_id: 52,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 28, column: 16 }..CharacterPosition { line: 28, column: 23 }, SearchPaths(vec![vec!["println".to_string()], vec!["std".to_string(), "io".to_string(), "println".to_string()]])),
                                        arguments: vec![Variable::Unnamed(51)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 53,
                                    variable_type: HIRType::Tuple(vec![]),
                                    expression: HIRExpression::ConstructTuple(vec![]),
                                },
                            ],
                            Variable::Unnamed(53)
                        ),
                    },
                },
            ]
        );
        println!("{}", statements_to_string(&statements));
        let statements = parser.parse(CODE2).expect("parse error");
        let KirlTopLevelItems { statements, .. } = collect_top_level_item_with_imports(statements);
        let statements = analysis_statements(statements).unwrap();
        assert_eq!(
            statements,
            vec![
                HIRStatement::Binding {
                    variable_id: 0,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(CharacterPosition { line: 3, column: 27 }..CharacterPosition { line: 3, column: 37 }, SearchPaths(vec![vec!["graph".to_string(), "get".to_string()]])),
                        arguments: vec![],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 1,
                    variable_type: HIRType::AnonymousStruct(vec![("nodes".to_string(), HIRType::Infer)].into_iter().collect()),
                    expression: HIRExpression::AccessVariable(Variable::Unnamed(0)),
                },
                HIRStatement::Binding {
                    variable_id: 2,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::AccessMember { variable: Variable::Unnamed(1), member: "nodes".to_string() },
                },
                HIRStatement::Binding {
                    variable_id: 3,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(
                            CharacterPosition { line: 4, column: 20 }..CharacterPosition { line: 4, column: 30 },
                            SearchPaths(vec![vec!["deque".to_string(), "new".to_string()], vec!["std".to_string(), "collections".to_string(), "deque".to_string(), "new".to_string()]]),
                        ),
                        arguments: vec![],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 4,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::AccessVariable(Variable::Unnamed(3)),
                },
                HIRStatement::Binding {
                    variable_id: 5,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::Immediate(Immediate::Number(0.into())),
                },
                HIRStatement::Binding {
                    variable_id: 6,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::CallFunction {
                        function: Variable::Named(
                            CharacterPosition { line: 5, column: 14 }..CharacterPosition { line: 5, column: 30 },
                            SearchPaths(vec![vec!["deque".to_string(), "push_back".to_string()], vec!["std".to_string(), "collections".to_string(), "deque".to_string(), "push_back".to_string()]]),
                        ),
                        arguments: vec![Variable::Unnamed(4), Variable::Unnamed(5)],
                    },
                },
                HIRStatement::Binding {
                    variable_id: 23,
                    variable_type: HIRType::Tuple(vec![]),
                    expression: HIRExpression::Loop(vec![
                        HIRStatement::Binding {
                            variable_id: 7,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(
                                    CharacterPosition { line: 6, column: 47 }..CharacterPosition { line: 6, column: 63 },
                                    SearchPaths(vec![vec!["deque".to_string(), "pop_front".to_string()], vec!["std".to_string(), "collections".to_string(), "deque".to_string(), "pop_front".to_string()]]),
                                ),
                                arguments: vec![Variable::Unnamed(4)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 10,
                            variable_type: HIRType::AnonymousStruct(vec![("value".to_string(), HIRType::Infer)].into_iter().collect()),
                            expression: HIRExpression::IfLet {
                                condition_binding: 8,
                                pattern_type: HIRType::AnonymousStruct(vec![("value".to_string(), HIRType::Infer)].into_iter().collect()),
                                condition: Variable::Unnamed(7),
                                then: (vec![], Variable::Unnamed(8)),
                                other: (
                                    vec![
                                        HIRStatement::Break(None),
                                        HIRStatement::Binding {
                                            variable_id: 9,
                                            variable_type: HIRType::Tuple(vec![]),
                                            expression: HIRExpression::ConstructTuple(vec![]),
                                        },
                                    ],
                                    Variable::Unnamed(9)
                                ),
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 11,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::AccessMember { variable: Variable::Unnamed(10), member: "value".to_string() },
                        },
                        HIRStatement::Binding {
                            variable_id: 12,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 7, column: 30 }..CharacterPosition { line: 7, column: 40 }, SearchPaths(vec![vec!["_get_item".to_string()]])),
                                arguments: vec![Variable::Unnamed(2), Variable::Unnamed(11)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 13,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 7, column: 16 }..CharacterPosition { line: 7, column: 23 }, SearchPaths(vec![vec!["println".to_string()], vec!["std".to_string(), "io".to_string(), "println".to_string()]])),
                                arguments: vec![Variable::Unnamed(12)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 14,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 8, column: 35 }..CharacterPosition { line: 8, column: 45 }, SearchPaths(vec![vec!["_get_item".to_string()]])),
                                arguments: vec![Variable::Unnamed(2), Variable::Unnamed(11)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 15,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 8, column: 47 }..CharacterPosition { line: 8, column: 62 }, SearchPaths(vec![vec!["graph".to_string(), "children".to_string()]])),
                                arguments: vec![Variable::Unnamed(14)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 16,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::CallFunction {
                                function: Variable::Named(CharacterPosition { line: 8, column: 29 }..CharacterPosition { line: 8, column: 64 }, SearchPaths(vec![vec!["_iterator".to_string()]])),
                                arguments: vec![Variable::Unnamed(15)],
                            },
                        },
                        HIRStatement::Binding {
                            variable_id: 22,
                            variable_type: HIRType::Tuple(vec![]),
                            expression: HIRExpression::Loop(vec![
                                HIRStatement::Binding {
                                    variable_id: 17,
                                    variable_type: HIRType::Or(vec![HIRType::Tuple(vec![]), HIRType::AnonymousStruct(vec![("value".to_string(), HIRType::Infer)].into_iter().collect())]),
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(CharacterPosition { line: 8, column: 29 }..CharacterPosition { line: 8, column: 64 }, SearchPaths(vec![vec!["_next".to_string()]])),
                                        arguments: vec![Variable::Unnamed(16)],
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 20,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::IfLet {
                                        condition_binding: 18,
                                        pattern_type: HIRType::AnonymousStruct(vec![("value".to_string(), HIRType::Infer)].into_iter().collect()),
                                        condition: Variable::Unnamed(17),
                                        then: (
                                            vec![HIRStatement::Binding {
                                                variable_id: 19,
                                                variable_type: HIRType::Infer,
                                                expression: HIRExpression::AccessMember { variable: Variable::Unnamed(18), member: "value".to_string() },
                                            }],
                                            Variable::Unnamed(18)
                                        ),
                                        other: (
                                            vec![
                                                HIRStatement::Break(None),
                                                HIRStatement::Binding {
                                                    variable_id: 20,
                                                    variable_type: HIRType::Tuple(vec![]),
                                                    expression: HIRExpression::ConstructTuple(vec![]),
                                                },
                                            ],
                                            Variable::Unnamed(20)
                                        ),
                                    },
                                },
                                HIRStatement::Binding {
                                    variable_id: 21,
                                    variable_type: HIRType::Infer,
                                    expression: HIRExpression::CallFunction {
                                        function: Variable::Named(
                                            CharacterPosition { line: 9, column: 22 }..CharacterPosition { line: 9, column: 38 },
                                            SearchPaths(vec![vec!["deque".to_string(), "push_back".to_string()], vec!["std".to_string(), "collections".to_string(), "deque".to_string(), "push_back".to_string()]]),
                                        ),
                                        arguments: vec![Variable::Unnamed(4), Variable::Unnamed(21)],
                                    },
                                },
                            ]),
                        },
                    ]),
                },
            ]
        );
        println!("{}", statements_to_string(&statements));
    }
}
