use kirl_common::dec::Decimal128;
use kirl_common::typing::HIRType;
use parser::enum_index_derive::*;
use parser::Symbol::{Error, NonTerminal, Terminal};
use parser::{enum_index, LR1Parser};
use parser_generator::parser;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::Range;

use crate::kirl_tokenizer::Token;
use crate::{CharacterPosition, ParseErrorDetail};

#[cfg(test)]
mod tests;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Statement {
    pub position: Range<CharacterPosition>,
    pub statement: StatementItem,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementItem {
    Import(ImportPath),
    Expression(Expression),
    LetBinding(LetBinding),
    Return(Option<Expression>),
    Continue(Option<String>),
    Break(Option<String>),
    For(ForStatement),
    While(WhileStatement),
}

impl Default for StatementItem {
    fn default() -> Self {
        StatementItem::Return(None)
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ForStatement {
    pub label: Option<String>,
    pub position: Range<CharacterPosition>,
    pub pattern: Pattern,
    pub iter: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    BoolExpression(Expression),
    LetBinding(LetBinding),
}

impl Default for Condition {
    fn default() -> Self {
        Condition::BoolExpression(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct WhileStatement {
    pub label: Option<String>,
    pub position: Range<CharacterPosition>,
    pub condition: Condition,
    pub block: Block,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Block {
    pub position: Range<CharacterPosition>,
    pub statements: Vec<Statement>,
    pub last_expression: Option<Box<Expression>>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Function {
    pub position: Range<CharacterPosition>,
    pub name: String,
    pub generics_arguments: Vec<String>,
    pub arguments: Vec<(Pattern, Type)>,
    pub return_type: Type,
    pub expression: Expression,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Struct {
    pub name: String,
    pub generics_arguments: Vec<String>,
    pub members: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ImportPath {
    Item(String),
    List(Vec<ImportPath>),
    Child(String, Box<ImportPath>),
}

impl Default for ImportPath {
    fn default() -> Self {
        ImportPath::Item(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Path {
    pub position: Range<CharacterPosition>,
    pub path: Vec<String>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct NamedType {
    pub position: Range<CharacterPosition>,
    pub path: Vec<String>,
    pub generics_arguments: Vec<Type>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct FunctionType {
    pub position: Range<CharacterPosition>,
    pub argument: Vec<Type>,
    pub result: Box<Type>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct AnonymousStructType {
    pub position: Range<CharacterPosition>,
    pub members: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    None,
    Unreachable(Range<CharacterPosition>),
    NamedType(NamedType),
    Tuple(Range<CharacterPosition>, Vec<Type>),
    Array(Range<CharacterPosition>, Box<Type>),
    Function(FunctionType),
    AnonymousStruct(AnonymousStructType),
    Or(Range<CharacterPosition>, Vec<Type>),
}

impl Default for Type {
    fn default() -> Self {
        Type::None
    }
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

impl std::error::Error for HIRTypeConvertError {}

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

#[derive(Debug, PartialEq, Clone)]
pub enum StructName {
    Anonymous,
    Named(NamedType),
}

impl Default for StructName {
    fn default() -> Self {
        StructName::Anonymous
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Expression {
    pub position: Range<CharacterPosition>,
    pub expression: ExpressionItem,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionItem {
    AccessVariable(Path, Vec<Type>),
    StringImmediate(String),
    NumberImmediate(Decimal128),
    AccessTupleItem(Box<Expression>, usize),
    AccessMember(Box<Expression>, String),
    CallFunction(FunctionReference, Vec<Expression>),
    Indexer(Box<Expression>, Box<Expression>),
    ConstructTuple(Vec<Expression>),
    ConstructArray(Vec<Expression>),
    ConstructStruct(ConstructStruct),
    Block(Block),
    Neg(Box<Expression>),
    // _neg
    Not(Box<Expression>),
    // _not
    Mul(Box<Expression>, Box<Expression>),
    // _mul
    Div(Box<Expression>, Box<Expression>),
    // _div
    Rem(Box<Expression>, Box<Expression>),
    // _rem
    Add(Box<Expression>, Box<Expression>),
    // _add
    Sub(Box<Expression>, Box<Expression>),
    // _sub
    GreaterThan(Box<Expression>, Box<Expression>),
    // _gt
    LessThan(Box<Expression>, Box<Expression>),
    // _gt (左右逆)
    GreaterOrEqual(Box<Expression>, Box<Expression>),
    // _not(_gt (左右逆))
    LessOrEqual(Box<Expression>, Box<Expression>),
    // _not(_gt)
    Equals(Box<Expression>, Box<Expression>),
    // _eq
    NotEquals(Box<Expression>, Box<Expression>),
    // _not(_eq)
    And(Box<Expression>, Box<Expression>),
    // _and
    Xor(Box<Expression>, Box<Expression>),
    // _xor
    Or(Box<Expression>, Box<Expression>),
    // _or
    Assign(Box<Expression>, Box<Expression>),
    Closure(Closure),
    If(If),
    Match(Match),
}

impl Default for ExpressionItem {
    fn default() -> Self {
        ExpressionItem::NumberImmediate(Default::default())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionReference {
    Dynamic(Box<Expression>),
    Static { path: Path, generics_type_arguments: Vec<Type> },
}

impl Default for FunctionReference {
    fn default() -> Self {
        FunctionReference::Dynamic(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ConstructStruct {
    pub name: StructName,
    pub items: Vec<(String, Expression)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum KirlTopLevelStatement {
    Statement((Range<CharacterPosition>, Statement)),
    FunctionDefinition((Range<CharacterPosition>, Function)),
    StructDefinition((Range<CharacterPosition>, Struct)),
}

impl Default for KirlTopLevelStatement {
    fn default() -> Self {
        KirlTopLevelStatement::Statement(Default::default())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Variable(String),
    Tuple(Vec<Pattern>),
    Struct(StructName, Vec<(String, Pattern)>),
}

impl Default for Pattern {
    fn default() -> Self {
        Pattern::Variable(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct LetBinding {
    pub position: Range<CharacterPosition>,
    pub pattern: Pattern,
    pub type_hint: Option<Type>,
    pub expression: Box<Expression>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Closure {
    pub position: Range<CharacterPosition>,
    pub arguments: Vec<(Pattern, Option<Type>)>,
    pub expression: Box<Expression>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct If {
    pub position: Range<CharacterPosition>,
    pub condition: Box<Condition>,
    pub then: Box<Expression>,
    pub other: Option<Box<Expression>>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Match {
    pub position: Range<CharacterPosition>,
    pub condition: Box<Expression>,
    pub items: Vec<(Pattern, Expression)>,
}

#[derive(Debug, EnumIndex, PartialEq, Clone)]
pub enum Symbol {
    ValidKirlCode((Range<CharacterPosition>, Vec<KirlTopLevelStatement>)),
    Type1((Range<CharacterPosition>, Type)),
    Type0((Range<CharacterPosition>, Type)),
    Type((Range<CharacterPosition>, Type)),
    TupleType((Range<CharacterPosition>, Vec<Type>)),
    FullPath((Range<CharacterPosition>, Path)),
    CommaSeparatedTypes((Range<CharacterPosition>, Vec<Type>)),
    OrSeparatedTypes((Range<CharacterPosition>, Vec<Type>)),
    StructDefinitionItems((Range<CharacterPosition>, Vec<(String, Type)>)),
    ConstructStruct((Range<CharacterPosition>, ConstructStruct)),
    StructName((Range<CharacterPosition>, StructName)),
    StructDefinition((Range<CharacterPosition>, Struct)),
    ConstructStructItems((Range<CharacterPosition>, Vec<(String, Expression)>)),
    LetBinding((Range<CharacterPosition>, LetBinding)),
    UnExpressionStatement((Range<CharacterPosition>, Statement)),
    Statement((Range<CharacterPosition>, Statement)),
    StatementList((Range<CharacterPosition>, Vec<Statement>)),
    ExpressionBlock((Range<CharacterPosition>, Block)),
    StatementBlock((Range<CharacterPosition>, Block)),
    VariableReference((Range<CharacterPosition>, Path, Vec<Type>)),
    CallFunctionArguments((Range<CharacterPosition>, Vec<Expression>)),
    Expression8((Range<CharacterPosition>, Expression)),
    Expression7((Range<CharacterPosition>, Expression)),
    Expression6((Range<CharacterPosition>, Expression)),
    Expression5((Range<CharacterPosition>, Expression)),
    Expression4((Range<CharacterPosition>, Expression)),
    Expression3((Range<CharacterPosition>, Expression)),
    Expression2((Range<CharacterPosition>, Expression)),
    Expression1((Range<CharacterPosition>, Expression)),
    Expression0((Range<CharacterPosition>, Expression)),
    Expression((Range<CharacterPosition>, Expression)),
    ClosureExpression((Range<CharacterPosition>, Closure)),
    IfExpression((Range<CharacterPosition>, If)),
    MatchExpression((Range<CharacterPosition>, Match)),
    MatchPatterns((Range<CharacterPosition>, Vec<(Pattern, Expression)>)),
    ForStatement((Range<CharacterPosition>, ForStatement)),
    WhileStatement((Range<CharacterPosition>, WhileStatement)),
    Pattern((Range<CharacterPosition>, Pattern)),
    CommaSeparatedPatterns((Range<CharacterPosition>, Vec<Pattern>)),
    PatternStructItems((Range<CharacterPosition>, Vec<(String, Pattern)>)),
    ClosureArguments((Range<CharacterPosition>, Vec<(Pattern, Option<Type>)>)),
    CommaSeparatedExpressions((Range<CharacterPosition>, Vec<Expression>)),
    FunctionDefinition((Range<CharacterPosition>, Function)),
    FunctionArguments((Range<CharacterPosition>, Vec<(Pattern, Type)>)),
    ImportStatement((Range<CharacterPosition>, ImportPath)),
    ImportPath((Range<CharacterPosition>, ImportPath)),
    ImportPathChild((Range<CharacterPosition>, ImportPath)),
    CommaSeparatedImportPathChildren((Range<CharacterPosition>, Vec<ImportPath>)),
    GenericsTypeArguments((Range<CharacterPosition>, Vec<String>)),
}

pub type Parser = LR1Parser<Symbol, Token, ParseErrorDetail>;

parser! {
    pub fn get_parser() -> LR1Parser {
        token Token{
            "import" = Import(Default::default()),
            "fn" = Fn(Default::default()),
            "struct" = Struct(Default::default()),
            "let" = Let(Default::default()),
            "var" = Var(Default::default()),
            "if" = If(Default::default()),
            "else" = Else(Default::default()),
            "match" = Match(Default::default()),
            "for" = For(Default::default()),
            "in" = In(Default::default()),
            "while" = While(Default::default()),
            "return" = Return(Default::default()),
            "break" = Break(Default::default()),
            "continue" = Continue(Default::default()),
            Identifier = Identifier(Default::default()),
            StringImmediate = StringImmediate(Default::default()),
            NumberImmediate = NumberImmediate(Default::default()),
            TupleIndex = TupleIndex(Default::default()),
            "!" = Not(Default::default()),
            "." = Dot(Default::default()),
            "," = Comma(Default::default()),
            "#" = Sharp(Default::default()),
            "::" = DoubleColon(Default::default()),
            ":" = Colon(Default::default()),
            ";" = Semicolon(Default::default()),
            ">" = GreaterThan(Default::default()),
            "<" = LessThan(Default::default()),
            ">=" = GreaterOrEqual(Default::default()),
            "<=" = LessOrEqual(Default::default()),
            "=" = Assign(Default::default()),
            "==" = Equals(Default::default()),
            "!=" = NotEquals(Default::default()),
            "+" = Add(Default::default()),
            "-" = Sub(Default::default()),
            "*" = Mul(Default::default()),
            "/" = Div(Default::default()),
            "%" = Rem(Default::default()),
            "&" = And(Default::default()),
            "|" = Or(Default::default()),
            "^" = Xor(Default::default()),
            "(" = RoundBracketOpen(Default::default()),
            ")" = RoundBracketClose(Default::default()),
            "[" = SquareBracketOpen(Default::default()),
            "]" = SquareBracketClose(Default::default()),
            "{" = WaveBracketOpen(Default::default()),
            "}" = WaveBracketClose(Default::default()),
            "->" = FunctionArrow(Default::default()),
            "|->" = MapsTo(Default::default()),
            "=>" = MatchArrow(Default::default()),
        }
        symbol Symbol{
            ValidKirlCode = ValidKirlCode(Default::default()),
            Type1 = Type1(Default::default()),
            Type0 = Type0(Default::default()),
            Type = Type(Default::default()),
            TupleType = TupleType(Default::default()),
            FullPath = FullPath(Default::default()),
            CommaSeparatedTypes = CommaSeparatedTypes(Default::default()),
            OrSeparatedTypes = OrSeparatedTypes(Default::default()),
            StructDefinitionItems = StructDefinitionItems(Default::default()),
            ConstructStruct = ConstructStruct(Default::default()),
            StructName = StructName(Default::default()),
            StructDefinition = StructDefinition(Default::default()),
            ConstructStructItems = ConstructStructItems(Default::default()),
            LetBinding = LetBinding(Default::default()),
            UnExpressionStatement = UnExpressionStatement(Default::default()),
            Statement = Statement(Default::default()),
            StatementList = StatementList(Default::default()),
            ExpressionBlock = ExpressionBlock(Default::default()),
            StatementBlock = StatementBlock(Default::default()),
            VariableReference = VariableReference(Default::default()),
            CallFunctionArguments = CallFunctionArguments(Default::default()),
            Expression8 = Expression8(Default::default()),
            Expression7 = Expression7(Default::default()),
            Expression6 = Expression6(Default::default()),
            Expression5 = Expression5(Default::default()),
            Expression4 = Expression4(Default::default()),
            Expression3 = Expression3(Default::default()),
            Expression2 = Expression2(Default::default()),
            Expression1 = Expression1(Default::default()),
            Expression0 = Expression0(Default::default()),
            Expression = Expression(Default::default()),
            ClosureExpression = ClosureExpression(Default::default()),
            IfExpression = IfExpression(Default::default()),
            MatchExpression = MatchExpression(Default::default()),
            MatchPatterns = MatchPatterns(Default::default()),
            ForStatement = ForStatement(Default::default()),
            WhileStatement = WhileStatement(Default::default()),
            Pattern = Pattern(Default::default()),
            CommaSeparatedPatterns = CommaSeparatedPatterns(Default::default()),
            PatternStructItems = PatternStructItems(Default::default()),
            ClosureArguments = ClosureArguments(Default::default()),
            CommaSeparatedExpressions = CommaSeparatedExpressions(Default::default()),
            FunctionDefinition = FunctionDefinition(Default::default()),
            FunctionArguments = FunctionArguments(Default::default()),
            ImportStatement = ImportStatement(Default::default()),
            ImportPath = ImportPath(Default::default()),
            ImportPathChild = ImportPathChild(Default::default()),
            CommaSeparatedImportPathChildren = CommaSeparatedImportPathChildren(Default::default()),
            GenericsTypeArguments = GenericsTypeArguments(Default::default()),
        }
        errortype ParseErrorDetail;
        start ValidKirlCode;
        <ValidKirlCode>::=: [] => Ok(Symbol::ValidKirlCode(Default::default()));
            |<ValidKirlCode> <Statement>: list => match list {
                    [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::UnExpressionStatement((range, statement)))] if list.is_empty() => Ok(Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::Statement((mem::take(range), mem::take(statement)))]))),
                    [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::Statement((range, statement)))] => Ok(Symbol::ValidKirlCode((*start..range.end, {
                        list.push(KirlTopLevelStatement::Statement((mem::take(range), mem::take(statement))));
                        mem::take(list)
                    }))),
                    _ => unreachable!(),
                };
            |<ValidKirlCode> <FunctionDefinition>: list => match list {
                    [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::FunctionDefinition((range, function)))] if list.is_empty() => Ok(Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::FunctionDefinition((mem::take(range), mem::take(function)))]))),
                    [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::FunctionDefinition((range, function)))] => Ok(Symbol::ValidKirlCode((*start..range.end, {
                        list.push(KirlTopLevelStatement::FunctionDefinition((mem::take(range), mem::take(function))));
                        mem::take(list)
                    }))),
                    _ => unreachable!(),
                };
            |<ValidKirlCode> <StructDefinition>: list => match list {
                    [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::StructDefinition((range, s)))] if list.is_empty() => Ok(Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::StructDefinition((mem::take(range), mem::take(s)))]))),
                    [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::StructDefinition((range, s)))] => Ok(Symbol::ValidKirlCode((*start..range.end, {
                        list.push(KirlTopLevelStatement::StructDefinition((mem::take(range), mem::take(s))));
                        mem::take(list)
                    }))),
                    _ => unreachable!(),
                };
            |<ValidKirlCode> ERROR: [_, Error(error_tokens)] => Err(ParseErrorDetail::SyntaxErrorAt(error_tokens.first().map(Token::get_position).map(|range|range.start).unwrap_or_default()..error_tokens.last().map(Token::get_position).map(|range|range.end).unwrap_or_default()));
        <Type1>::=<FullPath>: [NonTerminal(Symbol::FullPath((position, Path { path, .. })))] => Ok(Symbol::Type1((position.clone(), Type::NamedType(NamedType { position: position.clone(), path: mem::take(path), generics_arguments: Vec::new() }))));
            |"!": [Terminal(Token::Not(position))] => Ok(Symbol::Type1((position.clone(), Type::Unreachable(position.clone()))));
            |<FullPath> "::" "<" <CommaSeparatedTypes> ">": [NonTerminal(Symbol::FullPath((Range { start, .. }, Path { path, .. }))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan(Range { end, .. }))] => Ok(Symbol::Type1((*start..*end, Type::NamedType(NamedType {
                        position: *start..*end,
                        path: mem::take(path),
                        generics_arguments: mem::take(types),
                    }))));
            |"[" <Type> "]": [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, inner_type))), Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Type1((*start..*end, Type::Array(*start..*end, Box::new(mem::take(inner_type))))));
            |"(" <Type> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, inner_type))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Type1((*start..*end, mem::take(inner_type))));
            |<TupleType>: [NonTerminal(Symbol::TupleType((position, tuple)))] => Ok(Symbol::Type1((position.clone(), Type::Tuple(position.clone(), mem::take(tuple)))));
            |"#" "{" <StructDefinitionItems> "}": [Terminal(Token::Sharp(Range { start, .. })), _, NonTerminal(Symbol::StructDefinitionItems((_, members))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::Type1((*start..*end, Type::AnonymousStruct(AnonymousStructType { position: *start..*end, members: mem::take(members) }))));
        <Type0>::=<Type1>: [NonTerminal(Symbol::Type1(item))] => Ok(Symbol::Type0(mem::take(item)));
            |<Type0> "|" <Type1>: [NonTerminal(Symbol::Type0((Range { start, .. }, ty0))), _, NonTerminal(Symbol::Type1((Range { end, .. }, ty1)))] => Ok(Symbol::Type0((*start..*end, match mem::take(ty0) {
                Type::Or(_, mut types) => {
                    types.push(mem::take(ty1));
                    Type::Or(*start..*end, types)
                }
                other => Type::Or(*start..*end, vec![other, mem::take(ty1)]),
            })));
        <Type>::=<Type0>: [NonTerminal(Symbol::Type0(item))] => Ok(Symbol::Type(mem::take(item)));
            |<TupleType> "->" <Type>: [NonTerminal(Symbol::TupleType((Range { start, .. }, argument_types))), _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] => Ok(Symbol::Type((*start..*end, Type::Function(FunctionType {
                    position: *start..*end,
                    argument: mem::take(argument_types),
                    result: Box::new(mem::take(return_type)),
                }))));
            |"(" <Type> ")" "->" <Type>: [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, argument_type))), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] => Ok(Symbol::Type((*start..*end, Type::Function(FunctionType {
                    position: *start..*end,
                    argument: vec![mem::take(argument_type)],
                    result: Box::new(mem::take(return_type)),
                }))));
        <TupleType>::="(" ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::TupleType((*start..*end, Vec::new())));
            |"(" <Type> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, ty))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::TupleType((*start..*end, vec![mem::take(ty)])));
            |"(" <CommaSeparatedTypes> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::TupleType((*start..*end, mem::take(types))));
            |"(" <CommaSeparatedTypes> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedTypes((_, types))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::TupleType((*start..*end, mem::take(types))));
        <FullPath>::=[Identifier]: [Terminal(Token::Identifier((range, first)))] => Ok(Symbol::FullPath((range.clone(), Path { position: range.clone(), path: vec![first.clone()] })));
            |<FullPath> "::" [Identifier]: [NonTerminal(Symbol::FullPath((Range { start, .. }, Path { path, .. }))), _, Terminal(Token::Identifier((Range { end, .. }, last)))] => Ok(Symbol::FullPath((*start..*end, Path {
                    position: *start..*end,
                    path: {
                        path.push(last.clone());
                        mem::take(path)
                    },
                })));
        <CommaSeparatedTypes>::=<Type> "," <Type>: [NonTerminal(Symbol::Type((Range { start, .. }, first))), _, NonTerminal(Symbol::Type((Range { end, .. }, second)))] => Ok(Symbol::CommaSeparatedTypes((*start..*end, vec![mem::take(first), mem::take(second)])));
            |<CommaSeparatedTypes> "," <Type>: [NonTerminal(Symbol::CommaSeparatedTypes((Range { start, .. }, types))), _, NonTerminal(Symbol::Type((Range { end, .. }, last)))] => Ok(Symbol::CommaSeparatedTypes((*start..*end, {
                    types.push(mem::take(last));
                    mem::take(types)
                })));
        <StructDefinitionItems>::=[Identifier] ":" <Type>: [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, item_type)))] => Ok(Symbol::StructDefinitionItems((*start..*end, vec![(name.clone(), mem::take(item_type))])));
            |<StructDefinitionItems> "," [Identifier] ":" <Type>: [NonTerminal(Symbol::StructDefinitionItems((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, item_type)))] => Ok(Symbol::StructDefinitionItems((*start..*end, {
                    list.push((name.clone(), mem::take(item_type)));
                    mem::take(list)
                })));
        <ConstructStruct>::=<StructName> "{" <ConstructStructItems> "}": [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::ConstructStructItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: mem::take(items) })));
            |<StructName> "{" <ConstructStructItems> "," "}": [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::ConstructStructItems((_, items))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: mem::take(items) })));
            |<StructName> "{" "}": [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: Vec::new() })));
        <StructName>::="#": [Terminal(Token::Sharp(position))] => Ok(Symbol::StructName((position.clone(), StructName::Anonymous)));
        <StructDefinition>::="struct" [Identifier] "{" "}": [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: Vec::new(), members: Vec::new() })));
            |"struct" [Identifier] "::" "<" <GenericsTypeArguments> ">" "{" "}": [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, types))), _, _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: mem::take(types), members: Vec::new() })));
            |"struct" [Identifier] "{" <StructDefinitionItems> "}": [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::StructDefinitionItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: Vec::new(), members: mem::take(items) })));
            |"struct" [Identifier] "::" "<" <GenericsTypeArguments> ">" "{" <StructDefinitionItems> "}": [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, types))), _, _, NonTerminal(Symbol::StructDefinitionItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: mem::take(types), members: mem::take(items) })));
        <ConstructStructItems>::=[Identifier] ":" <Expression>: [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::ConstructStructItems((*start..*end, vec![(name.clone(), mem::take(expression))])));
            |[Identifier]: [Terminal(Token::Identifier((position, name)))] => Ok(Symbol::ConstructStructItems((position.clone(), vec![(name.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![name.clone()] }, Vec::new()) })])));
            |<ConstructStructItems> "," [Identifier] ":" <Expression>: [NonTerminal(Symbol::ConstructStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::ConstructStructItems((*start..*end, {
                    items.push((name.clone(), mem::take(expression)));
                    mem::take(items)
                })));
            |<ConstructStructItems> "," [Identifier]: [NonTerminal(Symbol::ConstructStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((position, name)))] => Ok(Symbol::ConstructStructItems((*start..position.end, {
                    items.push((name.clone(), Expression {
                        position: position.clone(),
                        expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![name.clone()] }, Vec::new()),
                    }));
                    mem::take(items)
                })));
        <LetBinding>::="var" <Pattern> "=" <Expression>: [Terminal(Token::Var(Range { start, .. })), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::LetBinding((*start..*end, LetBinding { position: *start..*end, pattern: mem::take(pattern), type_hint: None, expression: Box::new(mem::take(expression)) })));
            |"var" <Pattern> ":" <Type> "=" <Expression>: [Terminal(Token::Var(Range { start, .. })), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Type((_, type_hint))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::LetBinding((*start..*end, LetBinding { position: *start..*end, pattern: mem::take(pattern), type_hint: Some(mem::take(type_hint)), expression: Box::new(mem::take(expression)) })));
        <UnExpressionStatement>::=<ImportStatement>: [NonTerminal(Symbol::ImportStatement((position, path)))] => Ok(Symbol::UnExpressionStatement((position.clone(), Statement { position: position.clone(), statement: StatementItem::Import(mem::take(path)) })));
            |<Expression> ";": [NonTerminal(Symbol::Expression((Range { start, .. }, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Expression(mem::take(expression)) })));
            |<LetBinding> ";": [NonTerminal(Symbol::LetBinding((Range { start, .. }, let_binding))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::LetBinding(mem::take(let_binding)) })));
            |"return" <Expression> ";": [Terminal(Token::Return(Range { start, .. })), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Return(Some(mem::take(expression))) })));
            |"return" ";": [Terminal(Token::Return(Range { start, .. })), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Return(None) })));
            |"continue" ";": [Terminal(Token::Continue(Range { start, .. })), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Continue(None) })));
            |"continue" [Identifier] ";": [Terminal(Token::Continue(Range { start, .. })), Terminal(Token::Identifier((_, label))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Continue(Some(label.clone())) })));
            |"break" ";": [Terminal(Token::Break(Range { start, .. })), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Break(None) })));
            |"break" [Identifier] ";": [Terminal(Token::Break(Range { start, .. })), Terminal(Token::Identifier((_, label))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::UnExpressionStatement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Break(Some(label.clone())) })));
            |<ForStatement>: [NonTerminal(Symbol::ForStatement((position, for_statement)))] => Ok(Symbol::UnExpressionStatement((position.clone(), Statement { position: position.clone(), statement: StatementItem::For(mem::take(for_statement)) })));
            |<WhileStatement>: [NonTerminal(Symbol::WhileStatement((position, while_statement)))] => Ok(Symbol::UnExpressionStatement((position.clone(), Statement { position: position.clone(), statement: StatementItem::While(mem::take(while_statement)) })));
        <Statement>::=<UnExpressionStatement>: [NonTerminal(Symbol::UnExpressionStatement(item))] => Ok(Symbol::Statement(mem::take(item)));
            |<IfExpression>: [NonTerminal(Symbol::IfExpression((range, expression)))] => Ok(Symbol::Statement((range.clone(), Statement { position: range.clone(), statement: StatementItem::Expression(Expression{ position: range.clone(), expression: ExpressionItem::If(mem::take(expression)) }) })));
            |<MatchExpression>: [NonTerminal(Symbol::MatchExpression((range, expression)))] => Ok(Symbol::Statement((range.clone(), Statement { position: range.clone(), statement: StatementItem::Expression(Expression { position: range.clone(), expression: ExpressionItem::Match(mem::take(expression)) }) })));
        <StatementList>::=<Statement>: [NonTerminal(Symbol::Statement((position, statement)))] => Ok(Symbol::StatementList((position.clone(), vec![mem::take(statement)])));
            |<StatementList> <Statement>: [NonTerminal(Symbol::StatementList((Range { start, .. }, list))), NonTerminal(Symbol::Statement((Range { end, .. }, statement)))] => Ok(Symbol::StatementList((*start..*end, {
                    list.push(mem::take(statement));
                    mem::take(list)
                })));
        <ExpressionBlock>::="{" <StatementList> <Expression> "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::StatementList((_, list))), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ExpressionBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: mem::take(list),
                    last_expression: Some(Box::new(mem::take(expression))),
                })));
            |"{" <StatementList> <UnExpressionStatement> "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::StatementList((_, list))), NonTerminal(Symbol::UnExpressionStatement((_, statement))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ExpressionBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: {
                        list.push(mem::take(statement));
                        mem::take(list)
                    },
                    last_expression: None,
                })));
            |"{" <Expression> "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ExpressionBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: Vec::new(),
                    last_expression: Some(Box::new(mem::take(expression))),
                })));
            |"{" <UnExpressionStatement> "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::UnExpressionStatement((_, statement))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ExpressionBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: vec![mem::take(statement)],
                    last_expression: None,
                })));
            |"{" "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ExpressionBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: Vec::new(),
                    last_expression: None,
                })));
        <StatementBlock>::="{" <StatementList> "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::StatementList((_, list))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::StatementBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: mem::take(list),
                    last_expression: None,
                })));
            |"{" "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::StatementBlock((*start..*end, Block {
                    position: *start..*end,
                    statements: Vec::new(),
                    last_expression: None,
                })));
        <VariableReference>::=<FullPath>: [NonTerminal(Symbol::FullPath((range, path)))] => Ok(Symbol::VariableReference((range.clone(), mem::take(path), Vec::new())));
            |<FullPath> "::" "<" ">": [NonTerminal(Symbol::FullPath((range, path))), ..] => Ok(Symbol::VariableReference((range.clone(), mem::take(path), Vec::new())));
            |<FullPath> "::" "<" <Type> ">": [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, _, NonTerminal(Symbol::Type((_, ty))), Terminal(Token::GreaterThan(Range { end, .. }))] => Ok(Symbol::VariableReference((*start..*end, mem::take(path), vec![mem::take(ty)])));
            |<FullPath> "::" "<" <Type> "," ">": [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, _, NonTerminal(Symbol::Type((_, ty))), _, Terminal(Token::GreaterThan(Range { end, .. }))] => Ok(Symbol::VariableReference((*start..*end, mem::take(path), vec![mem::take(ty)])));
            |<FullPath> "::" "<" <CommaSeparatedTypes> ">": [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan(Range { end, .. }))] => Ok(Symbol::VariableReference((*start..*end, mem::take(path), mem::take(types))));
            |<FullPath> "::" "<" <CommaSeparatedTypes> "," ">": [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), _, Terminal(Token::GreaterThan(Range { end, .. }))] => Ok(Symbol::VariableReference((*start..*end, mem::take(path), mem::take(types))));
        <CallFunctionArguments>::="(" ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::CallFunctionArguments((*start..*end, Vec::new())));
            |"(" <Expression> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::CallFunctionArguments((*start..*end, vec![mem::take(expression)])));
            |"(" <Expression> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, expression))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::CallFunctionArguments((*start..*end, vec![mem::take(expression)])));
            |"(" <CommaSeparatedExpressions> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::CallFunctionArguments((*start..*end, mem::take(expressions))));
            |"(" <CommaSeparatedExpressions> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::CallFunctionArguments((*start..*end, mem::take(expressions))));
        <Expression8>::=<VariableReference>: [NonTerminal(Symbol::VariableReference((position, path, types)))] => Ok(Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(mem::take(path), mem::take(types)) })));
            |[StringImmediate]: [Terminal(Token::StringImmediate((position, value)))] => Ok(Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::StringImmediate(value.clone()) })));
            |[NumberImmediate]: [Terminal(Token::NumberImmediate((position, value)))] => Ok(Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::NumberImmediate(*value) })));
            |<VariableReference> <CallFunctionArguments>: [NonTerminal(Symbol::VariableReference((Range { start, .. }, path, types))), NonTerminal(Symbol::CallFunctionArguments((Range { end, .. }, expressions)))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(FunctionReference::Static { path: mem::take(path), generics_type_arguments: mem::take(types)}, mem::take(expressions)) })));
            |<Expression8> "." <VariableReference> <CallFunctionArguments>: [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::VariableReference((_, path, types))), NonTerminal(Symbol::CallFunctionArguments((Range { end, .. }, expressions)))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(FunctionReference::Static { path: mem::take(path), generics_type_arguments: mem::take(types)}, [mem::take(expression)].into_iter().chain(mem::take(expressions)).collect()) })));
            |<Expression8> "." <CallFunctionArguments>: [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::CallFunctionArguments((Range { end, .. }, expressions)))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(FunctionReference::Dynamic(Box::new(mem::take(expression))), mem::take(expressions)) })));
            |<Expression8> "." [Identifier]: [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::Identifier((Range { end, .. }, member)))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::AccessMember(Box::new(mem::take(expression)), member.clone()) })));
            |<Expression8> "." [TupleIndex]: [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::TupleIndex((Range { end, .. }, index)))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::AccessTupleItem(Box::new(mem::take(expression)), *index) })));
            |<Expression8> "[" <Expression> "]": [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::Expression((_, index))), Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Indexer(Box::new(mem::take(expression)), Box::new(mem::take(index))) })));
            |"(" <CommaSeparatedExpressions> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(mem::take(expressions)) })));
            |"(" <CommaSeparatedExpressions> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(mem::take(expressions)) })));
            |"[" <CommaSeparatedExpressions> "]": [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(mem::take(expressions)) })));
            |"[" <CommaSeparatedExpressions> "," "]": [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), _, Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(mem::take(expressions)) })));
            |"(" <Expression> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(vec![mem::take(item)]) })));
            |"[" <Expression> "]": [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(vec![mem::take(item)]) })));
            |"[" <Expression> "," "]": [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), _, Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(vec![mem::take(item)]) })));
            |"(" ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(Vec::new()) })));
            |"[" "]": [Terminal(Token::SquareBracketOpen(Range { start, .. })), Terminal(Token::SquareBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(Vec::new()) })));
            |"(" <Expression> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Expression8((*start..*end, mem::take(item))));
            |<ExpressionBlock>: [NonTerminal(Symbol::ExpressionBlock((position, block)))] => Ok(Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Block(mem::take(block)) })));
            |<ConstructStruct>: [NonTerminal(Symbol::ConstructStruct((position, item)))] => Ok(Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::ConstructStruct(mem::take(item)) })));
        <Expression7>::=<Expression8>: [NonTerminal(Symbol::Expression8(item))] => Ok(Symbol::Expression7(mem::take(item)));
            |"-" <Expression7>: [Terminal(Token::Sub(Range { start, .. })), NonTerminal(Symbol::Expression7((Range { end, .. }, expression)))] => Ok(Symbol::Expression7((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Neg(Box::new(mem::take(expression))) })));
            |"!" <Expression7>: [Terminal(Token::Not(Range { start, .. })), NonTerminal(Symbol::Expression7((Range { end, .. }, expression)))] => Ok(Symbol::Expression7((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Not(Box::new(mem::take(expression))) })));
        <Expression6>::=<Expression7>: [NonTerminal(Symbol::Expression7(item))] => Ok(Symbol::Expression6(mem::take(item)));
            |<Expression6> "*" <Expression7>: [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] => Ok(Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Mul(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression6> "/" <Expression7>: [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] => Ok(Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Div(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression6> "%" <Expression7>: [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] => Ok(Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Rem(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression5>::=<Expression6>: [NonTerminal(Symbol::Expression6(item))] => Ok(Symbol::Expression5(mem::take(item)));
            |<Expression5> "+" <Expression6>: [NonTerminal(Symbol::Expression5((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression6((Range { end, .. }, second)))] => Ok(Symbol::Expression5((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Add(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression5> "-" <Expression6>: [NonTerminal(Symbol::Expression5((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression6((Range { end, .. }, second)))] => Ok(Symbol::Expression5((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Sub(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression4>::=<Expression5>: [NonTerminal(Symbol::Expression5(item))] => Ok(Symbol::Expression4(mem::take(item)));
            |<Expression4> ">" <Expression5>: [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] => Ok(Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::GreaterThan(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression4> "<" <Expression5>: [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] => Ok(Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::LessThan(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression4> ">=" <Expression5>: [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] => Ok(Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::GreaterOrEqual(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression4> "<=" <Expression5>: [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] => Ok(Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::LessOrEqual(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression3>::=<Expression4>: [NonTerminal(Symbol::Expression4(item))] => Ok(Symbol::Expression3(mem::take(item)));
            |<Expression3> "==" <Expression4>: [NonTerminal(Symbol::Expression3((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression4((Range { end, .. }, second)))] => Ok(Symbol::Expression3((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Equals(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
            |<Expression3> "!=" <Expression4>: [NonTerminal(Symbol::Expression3((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression4((Range { end, .. }, second)))] => Ok(Symbol::Expression3((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::NotEquals(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression2>::=<Expression3>: [NonTerminal(Symbol::Expression3(item))] => Ok(Symbol::Expression2(mem::take(item)));
            |<Expression2> "&" <Expression3>: [NonTerminal(Symbol::Expression2((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression3((Range { end, .. }, second)))] => Ok(Symbol::Expression2((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::And(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression1>::=<Expression2>: [NonTerminal(Symbol::Expression2(item))] => Ok(Symbol::Expression1(mem::take(item)));
            |<Expression1> "^" <Expression2>: [NonTerminal(Symbol::Expression1((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression2((Range { end, .. }, second)))] => Ok(Symbol::Expression1((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Xor(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression0>::=<Expression1>: [NonTerminal(Symbol::Expression1(item))] => Ok(Symbol::Expression0(mem::take(item)));
            |<Expression0> "|" <Expression1>: [NonTerminal(Symbol::Expression0((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression1((Range { end, .. }, second)))] => Ok(Symbol::Expression0((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Or(Box::new(mem::take(first)), Box::new(mem::take(second))) })));
        <Expression>::=<Expression0>: [NonTerminal(Symbol::Expression0(item))] => Ok(Symbol::Expression(mem::take(item)));
            |<ClosureExpression>: [NonTerminal(Symbol::ClosureExpression((position, closure)))] => Ok(Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Closure(mem::take(closure)) })));
            |<IfExpression>: [NonTerminal(Symbol::IfExpression((position, expression)))] => Ok(Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::If(mem::take(expression)) })));
            |<MatchExpression>: [NonTerminal(Symbol::MatchExpression((position, expression)))] => Ok(Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Match(mem::take(expression)) })));
            |<Expression8> "=" <Expression>: [NonTerminal(Symbol::Expression8((Range { start, .. }, variable))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::Expression((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Assign(Box::new(mem::take(variable)), Box::new(mem::take(expression))) })));
        <ClosureExpression>::="fn" ":" <ClosureArguments> "|->" <Expression>: [Terminal(Token::Fn(Range { start, .. })), _, NonTerminal(Symbol::ClosureArguments((_, arguments))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::ClosureExpression((*start..*end, Closure { position: *start..*end, arguments: mem::take(arguments), expression: Box::new(mem::take(expression)) })));
            |"fn" ":" "|->" <Expression>: [Terminal(Token::Fn(Range { start, .. })), _, _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::ClosureExpression((*start..*end, Closure { position: *start..*end, arguments: Vec::new(), expression: Box::new(mem::take(expression)) })));
        <IfExpression>::="if" <Expression> <ExpressionBlock>: [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, block)))] => Ok(Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: block.position.clone(), expression: ExpressionItem::Block(mem::take(block)) }), other: None })));
            |"if" <Expression> <ExpressionBlock> "else" <ExpressionBlock>: [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::ExpressionBlock((_, then))), _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, other)))] => Ok(Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::Block(mem::take(other)) })) })));
            |"if" <Expression> <ExpressionBlock> "else" <IfExpression>: [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::ExpressionBlock((_, then))), _, NonTerminal(Symbol::IfExpression((Range { end, .. }, expression)))] => Ok(Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: expression.position.clone(), expression: ExpressionItem::If(mem::take(expression)) })) })));
            |"if" <LetBinding> <ExpressionBlock>: [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, then)))] => Ok(Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: None })));
            |"if" <LetBinding> <ExpressionBlock> "else" <ExpressionBlock>: [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::ExpressionBlock((_, then))), _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, other)))] => Ok(Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::Block(mem::take(other)) })) })));
            |"if" <LetBinding> <ExpressionBlock> "else" <IfExpression>: [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::ExpressionBlock((_, then))), _, NonTerminal(Symbol::IfExpression((Range { end, .. }, other)))] => Ok(Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::If(mem::take(other)) })) })));
        <MatchExpression>::="match" <Expression> "{" <MatchPatterns> "}": [Terminal(Token::Match(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), _, NonTerminal(Symbol::MatchPatterns((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::MatchExpression((*start..*end, Match { position: *start..*end, condition: Box::new(mem::take(condition)), items: mem::take(items) })));
            |"match" <Expression> "{" <MatchPatterns> "," "}": [Terminal(Token::Match(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), _, NonTerminal(Symbol::MatchPatterns((_, items))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::MatchExpression((*start..*end, Match { position: *start..*end, condition: Box::new(mem::take(condition)), items: mem::take(items) })));
        <MatchPatterns>::=<Pattern> "=>" <Expression>: [NonTerminal(Symbol::Pattern((Range { start, .. }, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::MatchPatterns((*start..*end, vec![(mem::take(pattern), mem::take(expression))])));
            |<MatchPatterns> "," <Pattern> "=>" <Expression>: [NonTerminal(Symbol::MatchPatterns((Range { start, .. }, patterns))), _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::MatchPatterns((*start..*end, {
                    patterns.push((mem::take(pattern), mem::take(expression)));
                    mem::take(patterns)
                })));
        <ForStatement>::="for" <Pattern> "in" <Expression> <StatementBlock>: [Terminal(Token::For(Range { start, .. })), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((_, iter))), NonTerminal(Symbol::StatementBlock((Range { end, .. }, block)))] => Ok(Symbol::ForStatement((*start..*end, ForStatement { label: None, position: *start..*end, pattern: mem::take(pattern), iter: mem::take(iter), block: mem::take(block) })));
            |[Identifier] ":" "for" <Pattern> "in" <Expression> <StatementBlock>: [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((_, iter))), NonTerminal(Symbol::StatementBlock((Range { end, .. }, block)))] => Ok(Symbol::ForStatement((*start..*end, ForStatement { label: Some(label.clone()), position: *start..*end, pattern: mem::take(pattern), iter: mem::take(iter), block: mem::take(block) })));
        <WhileStatement>::="while" <Expression> <StatementBlock>: [Terminal(Token::While(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::StatementBlock((Range { end, .. }, block)))] => Ok(Symbol::WhileStatement((*start..*end, WhileStatement { label: None, position: *start..*end, condition: Condition::BoolExpression(mem::take(condition)), block: mem::take(block) })));
            |"while" <LetBinding> <StatementBlock>: [Terminal(Token::While(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::StatementBlock((Range { end, .. }, block)))] => Ok(Symbol::WhileStatement((*start..*end, WhileStatement { label: None, position: *start..*end, condition: Condition::LetBinding(mem::take(condition)), block: mem::take(block) })));
            |[Identifier] ":" "while" <Expression> <StatementBlock>: [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::StatementBlock((Range { end, .. }, block)))] => Ok(Symbol::WhileStatement((*start..*end, WhileStatement { label: Some(label.clone()), position: *start..*end, condition: Condition::BoolExpression(mem::take(condition)), block: mem::take(block) })));
            |[Identifier] ":" "while" <LetBinding> <StatementBlock>: [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::StatementBlock((Range { end, .. }, block)))] => Ok(Symbol::WhileStatement((*start..*end, WhileStatement { label: Some(label.clone()), position: *start..*end, condition: Condition::LetBinding(mem::take(condition)), block: mem::take(block) })));
        <Pattern>::=[Identifier]: [Terminal(Token::Identifier((position, pattern)))] => Ok(Symbol::Pattern((position.clone(), Pattern::Variable(pattern.clone()))));
            |"(" <Pattern> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Pattern((_, inner_pattern))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Pattern((*start..*end, mem::take(inner_pattern))));
            |"(" <CommaSeparatedPatterns> ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedPatterns((_, patterns))), Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Pattern((*start..*end, Pattern::Tuple(mem::take(patterns)))));
            |"(" <CommaSeparatedPatterns> "," ")": [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedPatterns((_, patterns))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] => Ok(Symbol::Pattern((*start..*end, Pattern::Tuple(mem::take(patterns)))));
            |<StructName> "{" "}": [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), Vec::new()))));
            |<StructName> "{" <PatternStructItems> "}": [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::PatternStructItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), mem::take(items)))));
            |<StructName> "{" <PatternStructItems> "," "}": [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::PatternStructItems((_, items))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), mem::take(items)))));
        <CommaSeparatedPatterns>::=<Pattern> "," <Pattern>: [NonTerminal(Symbol::Pattern((Range { start, ..}, pattern1))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern2)))] => Ok(Symbol::CommaSeparatedPatterns((*start..*end, vec![mem::take(pattern1), mem::take(pattern2)])));
            |<CommaSeparatedPatterns> "," <Pattern>: [NonTerminal(Symbol::CommaSeparatedPatterns((Range { start, ..}, patterns))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] => {
                    patterns.push(mem::take(pattern));
                    Ok(Symbol::CommaSeparatedPatterns((*start..*end, mem::take(patterns))))
                };
        <PatternStructItems>::=[Identifier]: [Terminal(Token::Identifier((position, name)))] => Ok(Symbol::PatternStructItems((position.clone(), vec![(name.clone(), Pattern::Variable(name.clone()))])));
            |[Identifier] ":" <Pattern>: [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] => Ok(Symbol::PatternStructItems((*start..*end, vec![(name.clone(), mem::take(pattern))])));
            |<PatternStructItems> "," [Identifier]: [NonTerminal(Symbol::PatternStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] => Ok(Symbol::PatternStructItems((*start..*end, {
                    items.push((name.clone(), Pattern::Variable(name.clone())));
                    mem::take(items)
                })));
            |<PatternStructItems> "," [Identifier] ":" <Pattern>: [NonTerminal(Symbol::PatternStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] => Ok(Symbol::PatternStructItems((*start..*end, {
                    items.push((name.clone(), mem::take(pattern)));
                    mem::take(items)
                })));
        <ClosureArguments>::=<Pattern>: [NonTerminal(Symbol::Pattern((position, pattern)))] => Ok(Symbol::ClosureArguments((position.clone(), vec![(mem::take(pattern), None)])));
            |<Pattern> ":" <Type>: [NonTerminal(Symbol::Pattern((Range { start, .. }, pattern))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] => Ok(Symbol::ClosureArguments((*start..*end, vec![(mem::take(pattern), Some(mem::take(t)))])));
            |<ClosureArguments> "," <Pattern>: [NonTerminal(Symbol::ClosureArguments((Range { start, .. }, list))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] => Ok(Symbol::ClosureArguments((*start..*end, {
                    list.push((mem::take(pattern), None));
                    mem::take(list)
                })));
            |<ClosureArguments> "," <Pattern> ":" <Type>: [NonTerminal(Symbol::ClosureArguments((Range { start, .. }, list))), _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] => Ok(Symbol::ClosureArguments((*start..*end, {
                    list.push((mem::take(pattern), Some(mem::take(t))));
                    mem::take(list)
                })));
        <CommaSeparatedExpressions>::=<Expression> "," <Expression>: [NonTerminal(Symbol::Expression((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression((Range { end, .. }, second)))] => Ok(Symbol::CommaSeparatedExpressions((*start..*end, vec![mem::take(first), mem::take(second)])));
            |<CommaSeparatedExpressions> "," <Expression>: [NonTerminal(Symbol::CommaSeparatedExpressions((Range { start, .. }, list))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] => Ok(Symbol::CommaSeparatedExpressions((*start..*end, {
                    list.push(mem::take(expression));
                    mem::take(list)
                })));
        <FunctionDefinition>::="fn" [Identifier] "(" <FunctionArguments> ")" "->" <Type> <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "(" <FunctionArguments> ")" <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> ")" "->" <Type> <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> ")" <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "(" <FunctionArguments> "," ")" "->" <Type> <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "(" <FunctionArguments> "," ")" <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> "," ")" "->" <Type> <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> "," ")" <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "(" ")" "->" <Type> <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "(" ")" <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" ")" "->" <Type> <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" ")" <ExpressionBlock>: [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, NonTerminal(Symbol::ExpressionBlock((Range { end, .. }, expression)))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } })));
            |"fn" [Identifier] "(" <FunctionArguments> ")" "->" <Type> "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) })));
            |"fn" [Identifier] "(" <FunctionArguments> ")" "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> ")" "->" <Type> "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> ")" "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) })));
            |"fn" [Identifier] "(" <FunctionArguments> "," ")" "->" <Type> "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Type((_, t))), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) })));
            |"fn" [Identifier] "(" <FunctionArguments> "," ")" "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> "," ")" "->" <Type> "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Type((_, t))), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" <FunctionArguments> "," ")" "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) })));
            |"fn" [Identifier] "(" ")" "->" <Type> "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Type((_, t))), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: mem::take(t), expression: mem::take(expression) })));
            |"fn" [Identifier] "(" ")" "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: Type::None, expression: mem::take(expression) })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" ")" "->" <Type> "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Type((_, t))), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: mem::take(t), expression: mem::take(expression) })));
            |"fn" [Identifier] "::" "<" <GenericsTypeArguments> ">" "(" ")" "|->" <Expression> ";": [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: Type::None, expression: mem::take(expression) })));
        <FunctionArguments>::=<Pattern> ":" <Type>: [NonTerminal(Symbol::Pattern((Range { start, .. }, pattern))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] => Ok(Symbol::FunctionArguments((*start..*end, vec![(mem::take(pattern), mem::take(t))])));
            |<FunctionArguments> "," <Pattern> ":" <Type>: [NonTerminal(Symbol::FunctionArguments((Range { start, .. }, list))), _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] => Ok(Symbol::FunctionArguments((*start..*end, {
                    list.push((mem::take(pattern), mem::take(t)));
                    mem::take(list)
                })));
        <ImportStatement>::="import" <ImportPath> ";": [Terminal(Token::Import(Range { start, .. })), NonTerminal(Symbol::ImportPath((_, path))), Terminal(Token::Semicolon(Range { end, .. }))] => Ok(Symbol::ImportStatement((*start..*end, mem::take(path))));
        <ImportPath>::=<ImportPathChild>: [NonTerminal(Symbol::ImportPathChild(item))] => Ok(Symbol::ImportPath(mem::take(item)));
            |[StringImmediate] "::" <ImportPathChild>: [Terminal(Token::StringImmediate((Range { start, .. }, name))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] => Ok(Symbol::ImportPath((*start..*end, ImportPath::Child(name.clone(), Box::new(mem::take(path))))));
        <ImportPathChild>::=[Identifier]: [Terminal(Token::Identifier((position, name)))] => Ok(Symbol::ImportPathChild((position.clone(), ImportPath::Item(name.clone()))));
            |[Identifier] "::" <ImportPathChild>: [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] => Ok(Symbol::ImportPathChild((*start..*end, ImportPath::Child(name.clone(), Box::new(mem::take(path))))));
            |"{" <CommaSeparatedImportPathChildren> "}": [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedImportPathChildren((_, paths))), Terminal(Token::WaveBracketClose(Range { end, .. }))] => Ok(Symbol::ImportPathChild((*start..*end, ImportPath::List(mem::take(paths)))));
        <CommaSeparatedImportPathChildren>::=<ImportPathChild>: [NonTerminal(Symbol::ImportPathChild((position, path)))] => Ok(Symbol::CommaSeparatedImportPathChildren((position.clone(), vec![mem::take(path)])));
            |<CommaSeparatedImportPathChildren> "," <ImportPathChild>: [NonTerminal(Symbol::CommaSeparatedImportPathChildren((Range { start, .. }, list))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] => Ok(Symbol::CommaSeparatedImportPathChildren((*start..*end, {
                    list.push(mem::take(path));
                    mem::take(list)
                })));
        <GenericsTypeArguments>::=[Identifier]: [Terminal(Token::Identifier((position, name)))] => Ok(Symbol::GenericsTypeArguments((position.clone(), vec![name.clone()])));
            |<GenericsTypeArguments> "," [Identifier]: [NonTerminal(Symbol::GenericsTypeArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] => Ok(Symbol::GenericsTypeArguments((*start..*end, {
                    list.push(name.clone());
                    mem::take(list)
                })));
    }
}
