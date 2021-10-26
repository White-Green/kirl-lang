use std::borrow::Cow;
use std::collections::BTreeMap;
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt::{Display, Formatter};

use dec::Decimal128;
use uuid::Uuid;

use kirl_semantic_analyzer::{HIRExpression, HIRStatement, HIRType, Immediate, ReferenceAccess, Variable};

#[derive(Debug, Clone, PartialEq, Ord, PartialOrd, Eq, Hash)]
pub enum LIRType {
    Unreachable,
    Named { path: Vec<String>, generics_arguments: Vec<LIRType> },
    Tuple(Vec<LIRType>),
    Array(Box<LIRType>),
    Function { arguments: Vec<LIRType>, result: Box<LIRType> },
    AnonymousStruct(BTreeMap<String, LIRType>),
    Or(Vec<LIRType>),
}

impl From<LIRType> for HIRType {
    fn from(value: LIRType) -> Self {
        match value {
            LIRType::Unreachable => HIRType::Unreachable,
            LIRType::Named { path, generics_arguments } => HIRType::Named {
                path,
                generics_arguments: generics_arguments.into_iter().map(Into::into).collect(),
            },
            LIRType::Tuple(items) => HIRType::Tuple(items.into_iter().map(Into::into).collect()),
            LIRType::Array(item) => HIRType::Array(Box::new((*item).into())),
            LIRType::Function { arguments, result } => HIRType::Function {
                arguments: arguments.into_iter().map(Into::into).collect(),
                result: Box::new((*result).into()),
            },
            LIRType::AnonymousStruct(items) => HIRType::AnonymousStruct(items.into_iter().map(|(k, v)| (k, v.into())).collect()),
            LIRType::Or(items) => HIRType::Or(items.into_iter().map(Into::into).collect()),
        }
    }
}

impl LIRType {
    pub fn is_a(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (LIRType::Unreachable, _) => true,
            (_, LIRType::Unreachable) => false,
            (LIRType::Named { path: path1, generics_arguments: arg1 }, LIRType::Named { path: path2, generics_arguments: arg2 }) => path1 == path2 && arg1.len() == arg2.len() && arg1.iter().zip(arg2).all(|(ty1, ty2)| ty1.is_a(ty2)),
            (LIRType::Tuple(items1), LIRType::Tuple(items2)) => items1.len() == items2.len() && items1.iter().zip(items2).all(|(ty1, ty2)| ty1.is_a(ty2)),
            (LIRType::Array(t1), LIRType::Array(t2)) => t1.is_a(t2),
            (LIRType::Function { arguments: arg1, result: res1 }, LIRType::Function { arguments: arg2, result: res2 }) => arg1.len() == arg2.len() && arg2.iter().zip(arg1).all(|(ty1, ty2)| ty1.is_a(ty2)) && res1.is_a(res2),
            (LIRType::AnonymousStruct(members1), LIRType::AnonymousStruct(members2)) => members2.iter().all(|(k, v2)| members1.get(k).map_or(false, |v1| v1.is_a(v2))),
            (LIRType::Or(items1), ty2) => items1.iter().all(|ty1| ty1.is_a(ty2)),
            (ty1, LIRType::Or(items2)) => items2.iter().any(|ty2| ty1.is_a(ty2)),
            _ => false,
        }
    }

    pub fn normalize(&mut self) {
        match self {
            LIRType::Tuple(items) => items.iter_mut().for_each(LIRType::normalize),
            LIRType::Array(item) => item.normalize(),
            LIRType::Function { arguments, result } => {
                arguments.iter_mut().for_each(LIRType::normalize);
                result.normalize();
            }
            LIRType::AnonymousStruct(members) => members.values_mut().for_each(LIRType::normalize),
            LIRType::Or(items) => {
                items.iter_mut().for_each(LIRType::normalize);
                *items = items.drain(..).flat_map(|ty| if let LIRType::Or(items) = ty { items } else { vec![ty] }).collect();
                items.sort();
                items.dedup();
                if items.len() > 1 {
                    items.retain(|ty| ty != &LIRType::Unreachable);
                }
                if items.len() == 1 {
                    *self = items.pop().unwrap();
                }
            }
            _ => {}
        }
    }

    pub fn into_normalized(mut self) -> LIRType {
        self.normalize();
        self
    }

    pub fn member_type(&self, member_name: &str) -> Option<Cow<LIRType>> {
        match self {
            LIRType::AnonymousStruct(members) => members.get(member_name).map(Cow::Borrowed),
            LIRType::Or(items) => {
                let mut result = Vec::with_capacity(items.len());
                for ty in items.iter().map(|ty| ty.member_type(member_name)) {
                    match ty {
                        None => return None,
                        Some(ty) => result.push(ty.into_owned()),
                    }
                }
                Some(Cow::Owned(LIRType::Or(result)))
            }
            _ => None,
        }
    }

    pub fn has_member(&self, member_name: &str) -> bool {
        match self {
            LIRType::AnonymousStruct(members) => members.contains_key(member_name),
            LIRType::Or(items) => items.iter().all(|ty| ty.has_member(member_name)),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum LIRTypeConvertError {
    UnSupportedType(&'static str),
}

impl Display for LIRTypeConvertError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LIRTypeConvertError::UnSupportedType(name) => write!(f, "HIR type {:?} is not supported.", name),
        }
    }
}

impl Error for LIRTypeConvertError {}

impl TryFrom<HIRType> for LIRType {
    type Error = LIRTypeConvertError;
    fn try_from(mut ty: HIRType) -> Result<Self, Self::Error> {
        ty.normalize();
        match ty {
            HIRType::Infer => Err(LIRTypeConvertError::UnSupportedType("Infer")),
            HIRType::Unreachable => Ok(LIRType::Unreachable),
            HIRType::Named { path, generics_arguments } => {
                let mut result_generics_arguments = Vec::with_capacity(generics_arguments.len());
                for hir_type in generics_arguments {
                    result_generics_arguments.push(hir_type.try_into()?);
                }
                Ok(LIRType::Named { path, generics_arguments: result_generics_arguments })
            }
            HIRType::Tuple(items) => {
                let mut result_items = Vec::with_capacity(items.len());
                for hir_type in items {
                    result_items.push(hir_type.try_into()?);
                }
                Ok(LIRType::Tuple(result_items))
            }
            HIRType::Array(item) => Ok(LIRType::Array(Box::new((*item).try_into()?))),
            HIRType::Function { arguments, result } => {
                let mut result_arguments = Vec::with_capacity(arguments.len());
                for hir_type in arguments {
                    result_arguments.push(hir_type.try_into()?);
                }
                Ok(LIRType::Function { arguments: result_arguments, result: Box::new((*result).try_into()?) })
            }
            HIRType::AnonymousStruct(members) => {
                let mut result_members = BTreeMap::new();
                for (member, hir_type) in members {
                    result_members.insert(member, hir_type.try_into()?);
                }
                Ok(LIRType::AnonymousStruct(result_members))
            }
            HIRType::Or(items) => {
                if items.is_empty() {
                    return Err(LIRTypeConvertError::UnSupportedType("Empty or-type"));
                }
                let mut result_items = Vec::with_capacity(items.len());
                for hir_type in items {
                    result_items.push(hir_type.try_into()?);
                }
                Ok(LIRType::Or(result_items))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LIRStatementList(pub Vec<LIRStatement>);

impl From<Vec<LIRStatement>> for LIRStatementList {
    fn from(value: Vec<LIRStatement>) -> Self {
        LIRStatementList(value)
    }
}

impl From<LIRStatementList> for Vec<LIRStatement> {
    fn from(value: LIRStatementList) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LIRStatement {
    pub(crate) label: Option<String>,
    pub(crate) instruction: LIRInstruction,
}

impl From<LIRInstruction> for LIRStatement {
    fn from(instruction: LIRInstruction) -> Self {
        LIRStatement { label: None, instruction }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LIRInstruction {
    LoadImmediateString(String),
    LoadImmediateNumber(Decimal128),
    LoadNamedValue(Uuid),
    Load(usize),
    Store(usize),
    JumpIfTrue(String),
    JumpIfHasType(LIRType, String),
    Jump(String),
    CallFunction(Uuid),
    Return,
    Nop,
    AccessMember(String),
    AccessTupleItem(usize),
    AssignMember(String),
    AssignTupleItem(usize),
    ConstructStruct(usize),
    ConstructTuple(usize),
    ConstructArray(usize),
}

#[derive(Debug)]
pub enum LIRStatementListConvertError {
    TypeConvertError(LIRTypeConvertError),
    UnexpectedBreak,
    UnexpectedContinue,
}

impl Display for LIRStatementListConvertError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LIRStatementListConvertError::TypeConvertError(e) => e.fmt(f),
            LIRStatementListConvertError::UnexpectedBreak => write!(f, "Unexpected break."),
            LIRStatementListConvertError::UnexpectedContinue => write!(f, "Unexpected continue."),
        }
    }
}

impl Error for LIRStatementListConvertError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            LIRStatementListConvertError::TypeConvertError(e) => Some(e),
            LIRStatementListConvertError::UnexpectedBreak => None,
            LIRStatementListConvertError::UnexpectedContinue => None,
        }
    }
}

impl From<LIRTypeConvertError> for LIRStatementListConvertError {
    fn from(e: LIRTypeConvertError) -> Self {
        LIRStatementListConvertError::TypeConvertError(e)
    }
}

// impl TryFrom<HIRStatementList<(Uuid, HIRType)>> for LIRStatementList {
//     type Error = LIRStatementListConvertError;
//     fn try_from(HIRStatementList(statements): HIRStatementList<(Uuid, HIRType)>) -> Result<Self, Self::Error> {
//         hir_to_lir(statements).map(Into::into)
//     }
// }

pub fn hir_to_lir(statements: Vec<HIRStatement<(Uuid, HIRType)>>, argument_count: usize) -> Result<LIRStatementList, LIRStatementListConvertError> {
    fn convert_list(statements: impl IntoIterator<Item = HIRStatement<(Uuid, HIRType)>>, result: &mut Vec<LIRStatement>, sequence: &mut usize, loop_labels: &mut Vec<String>) -> Result<(), LIRStatementListConvertError> {
        fn convert(statement: HIRStatement<(Uuid, HIRType)>, result: &mut Vec<LIRStatement>, sequence: &mut usize, loop_labels: &mut Vec<String>) -> Result<(), LIRStatementListConvertError> {
            fn push_variable(variable: Variable<(Uuid, HIRType)>, result: &mut Vec<LIRStatement>) {
                match variable {
                    Variable::Named(_, (id, _)) => result.push(LIRInstruction::LoadNamedValue(id).into()),
                    Variable::Unnamed(id) => result.push(LIRInstruction::Load(id).into()),
                }
            }
            match statement {
                HIRStatement::Binding { variable_id, expression, .. } => {
                    match expression {
                        HIRExpression::Immediate(value) => match value {
                            Immediate::Number(value) => result.push(LIRInstruction::LoadImmediateNumber(value).into()),
                            Immediate::String(value) => result.push(LIRInstruction::LoadImmediateString(value).into()),
                        },
                        HIRExpression::CallFunction { function, arguments } => {
                            let function = match function {
                                Variable::Named(_, (id, _)) => id,
                                Variable::Unnamed(_) => todo!(),
                            };
                            for variable in arguments.into_iter().rev() {
                                push_variable(variable, result);
                            }
                            result.push(LIRInstruction::CallFunction(function).into());
                        }
                        HIRExpression::AccessVariable(variable) => {
                            push_variable(variable, result);
                        }
                        HIRExpression::AccessTupleItem { variable, index } => {
                            push_variable(variable, result);
                            result.push(LIRInstruction::AccessTupleItem(index).into());
                        }
                        HIRExpression::AccessMember { variable, member } => {
                            push_variable(variable, result);
                            result.push(LIRInstruction::AccessMember(member).into());
                        }
                        HIRExpression::If {
                            condition,
                            then: (then_statements, then_result),
                            other: (other_statements, other_result),
                        } => {
                            push_variable(condition, result);
                            let then_label = format!("$if_then_{}", *sequence);
                            let end_label = format!("$if_end_{}", *sequence);
                            *sequence += 1;
                            result.push(LIRInstruction::JumpIfTrue(then_label.clone()).into());
                            convert_list(other_statements, result, sequence, loop_labels)?;
                            push_variable(other_result, result);
                            result.push(LIRInstruction::Jump(end_label.clone()).into());
                            result.push(LIRStatement { label: Some(then_label), instruction: LIRInstruction::Nop });
                            convert_list(then_statements, result, sequence, loop_labels)?;
                            push_variable(then_result, result);
                            result.push(LIRStatement { label: Some(end_label), instruction: LIRInstruction::Nop });
                        }
                        HIRExpression::IfLet {
                            condition_binding,
                            pattern_type,
                            condition,
                            then: (then_statements, then_result),
                            other: (other_statements, other_result),
                        } => {
                            push_variable(condition, result);
                            let then_label = format!("$if_then_{}", *sequence);
                            let end_label = format!("$if_end_{}", *sequence);
                            *sequence += 1;
                            result.push(LIRInstruction::JumpIfHasType(pattern_type.try_into()?, then_label.clone()).into());
                            convert_list(other_statements, result, sequence, loop_labels)?;
                            push_variable(other_result, result);
                            result.push(LIRInstruction::Jump(end_label.clone()).into());
                            result.push(LIRStatement {
                                label: Some(then_label),
                                instruction: LIRInstruction::Store(condition_binding),
                            });
                            convert_list(then_statements, result, sequence, loop_labels)?;
                            push_variable(then_result, result);
                            result.push(LIRStatement { label: Some(end_label), instruction: LIRInstruction::Nop });
                        }
                        HIRExpression::Loop(inner) => {
                            let label = format!("$loop_anonymous_{}", *sequence);
                            let label_begin = format!("{}_begin", label);
                            let label_end = format!("{}_end", label);
                            loop_labels.push(label);
                            *sequence += 1;
                            result.push(LIRStatement { label: Some(label_begin.clone()), instruction: LIRInstruction::Nop });
                            convert_list(inner, result, sequence, loop_labels)?;
                            result.push(LIRInstruction::Jump(label_begin).into());
                            result.push(LIRStatement { label: Some(label_end), instruction: LIRInstruction::Nop });
                        }
                        HIRExpression::Assign { variable, value } => {
                            push_variable(value.clone(), result);
                            match variable {
                                ReferenceAccess::Variable(dest) => {
                                    push_variable(value, result);
                                    match dest {
                                        Variable::Named(_, _) => todo!(),
                                        Variable::Unnamed(dest) => result.push(LIRInstruction::Store(dest).into()),
                                    }
                                }
                                ReferenceAccess::TupleItem(dest_variable, dest_index) => {
                                    push_variable(dest_variable, result);
                                    push_variable(value, result);
                                    result.push(LIRInstruction::AssignTupleItem(dest_index).into());
                                }
                                ReferenceAccess::Member(dest_variable, dest_member) => {
                                    push_variable(dest_variable, result);
                                    push_variable(value, result);
                                    result.push(LIRInstruction::AssignMember(dest_member).into());
                                }
                            }
                        }
                        HIRExpression::ConstructStruct(members) => {
                            let len = members.len();
                            for (member, value) in members {
                                push_variable(value, result);
                                result.push(LIRInstruction::LoadImmediateString(member).into());
                            }
                            result.push(LIRInstruction::ConstructStruct(len).into());
                        }
                        HIRExpression::ConstructTuple(items) => {
                            let len = items.len();
                            for item in items.into_iter().rev() {
                                push_variable(item, result);
                            }
                            result.push(LIRInstruction::ConstructTuple(len).into());
                        }
                        HIRExpression::ConstructArray(items) => {
                            let len = items.len();
                            for item in items.into_iter().rev() {
                                push_variable(item, result);
                            }
                            result.push(LIRInstruction::ConstructTuple(len).into());
                        }
                    }
                    result.push(LIRInstruction::Store(variable_id).into());
                }
                HIRStatement::Return(value) => {
                    push_variable(value, result);
                    result.push(LIRInstruction::Return.into());
                }
                HIRStatement::Continue(label) => {
                    if let Some(label) = label.as_ref().or_else(|| loop_labels.last()) {
                        result.push(LIRInstruction::Jump(format!("{}_begin", label)).into());
                    } else {
                        return Err(LIRStatementListConvertError::UnexpectedContinue);
                    }
                }
                HIRStatement::Break(label) => {
                    result.push(LIRInstruction::ConstructTuple(0).into()); //TODO:loop式とかで値を持ってbreakするときに変える
                    if let Some(label) = label.as_ref().or_else(|| loop_labels.last()) {
                        result.push(LIRInstruction::Jump(format!("{}_end", label)).into());
                    } else {
                        return Err(LIRStatementListConvertError::UnexpectedBreak);
                    }
                }
            }
            Ok(())
        }
        for statement in statements {
            convert(statement, result, sequence, loop_labels)?;
        }
        Ok(())
    }
    let mut result = Vec::new();
    for i in 0..argument_count {
        result.push(LIRInstruction::Store(i).into());
    }
    let mut loop_labels = Vec::new();
    convert_list(statements, &mut result, &mut 0, &mut loop_labels)?;
    result.push(LIRInstruction::ConstructTuple(0).into());
    result.push(LIRInstruction::Return.into());
    Ok(result.into())
}
