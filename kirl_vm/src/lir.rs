use std::collections::BTreeMap;
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt::{Display, Formatter};

use uuid::Uuid;

use kirl_semantic_analyzer::{HIRExpression, HIRStatement, HIRStatementList, HIRType, Immediate, ReferenceAccess, Variable};

#[derive(Debug, Clone, PartialEq)]
pub enum LIRType {
    None,
    Unreachable,
    Named { path: Vec<String>, generics_arguments: Vec<LIRType> },
    Tuple(Vec<LIRType>),
    Array(Box<LIRType>),
    Function { arguments: Vec<LIRType>, result: Box<LIRType> },
    AnonymousStruct(BTreeMap<String, LIRType>),
    Or(Vec<LIRType>),
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
            HIRType::None => Ok(LIRType::None),
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
    LoadImmediateNumber(()),
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
    AssignMember(String),
    ConstructStruct(usize),
    ConstructTuple(usize),
    ConstructArray(usize),
}

#[derive(Debug)]
pub enum LIRStatementListConvertError {
    TypeConvertError(LIRTypeConvertError),
}

impl Display for LIRStatementListConvertError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LIRStatementListConvertError::TypeConvertError(e) => e.fmt(f),
        }
    }
}

impl Error for LIRStatementListConvertError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            LIRStatementListConvertError::TypeConvertError(e) => Some(e),
        }
    }
}

impl From<LIRTypeConvertError> for LIRStatementListConvertError {
    fn from(e: LIRTypeConvertError) -> Self {
        LIRStatementListConvertError::TypeConvertError(e)
    }
}

impl TryFrom<HIRStatementList<(Uuid, HIRType)>> for LIRStatementList {
    type Error = LIRStatementListConvertError;
    fn try_from(HIRStatementList(statements): HIRStatementList<(Uuid, HIRType)>) -> Result<Self, Self::Error> {
        hir_to_lir(statements).map(Into::into)
    }
}

fn hir_to_lir(statements: Vec<HIRStatement<(Uuid, HIRType)>>) -> Result<Vec<LIRStatement>, LIRStatementListConvertError> {
    fn convert_list(statements: impl IntoIterator<Item = HIRStatement<(Uuid, HIRType)>>, result: &mut Vec<LIRStatement>, sequence: &mut usize) -> Result<(), LIRStatementListConvertError> {
        fn convert(statement: HIRStatement<(Uuid, HIRType)>, result: &mut Vec<LIRStatement>, sequence: &mut usize) -> Result<(), LIRStatementListConvertError> {
            fn push_variable(variable: Variable<(Uuid, HIRType)>, result: &mut Vec<LIRStatement>) {
                match variable {
                    Variable::Named((id, _)) => result.push(LIRInstruction::LoadNamedValue(id).into()),
                    Variable::Unnamed(id) => result.push(LIRInstruction::Load(id).into()),
                }
            }
            match statement {
                HIRStatement::Binding { variable_id, expression, .. } => {
                    match expression {
                        HIRExpression::Immediate(value) => match value {
                            Immediate::Integer(_) => {
                                todo!()
                            }
                            Immediate::Float(_) => {
                                todo!()
                            }
                            Immediate::String(value) => result.push(LIRInstruction::LoadImmediateString(value).into()),
                        },
                        HIRExpression::CallFunction { function, arguments } => {
                            let function = match function {
                                Variable::Named((id, _)) => id,
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
                            convert_list(other_statements, result, sequence)?;
                            push_variable(other_result, result);
                            result.push(LIRInstruction::Jump(end_label.clone()).into());
                            result.push(LIRStatement { label: Some(then_label), instruction: LIRInstruction::Nop });
                            convert_list(then_statements, result, sequence)?;
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
                            convert_list(other_statements, result, sequence)?;
                            push_variable(other_result, result);
                            result.push(LIRInstruction::Jump(end_label.clone()).into());
                            result.push(LIRStatement { label: Some(then_label), instruction: LIRInstruction::Nop });
                            result.push(LIRInstruction::Store(condition_binding).into());
                            convert_list(then_statements, result, sequence)?;
                            push_variable(then_result, result);
                            result.push(LIRStatement { label: Some(end_label), instruction: LIRInstruction::Nop });
                        }
                        HIRExpression::Loop(inner) => {
                            let label = format!("$loop_anonymous_{}", *sequence);
                            *sequence += 1;
                            result.push(LIRStatement { label: Some(label.clone()), instruction: LIRInstruction::Nop });
                            convert_list(inner, result, sequence)?;
                            result.push(LIRInstruction::Jump(label).into());
                        }
                        HIRExpression::Assign { variable, value } => match variable {
                            ReferenceAccess::Variable(dest) => {
                                push_variable(value, result);
                                match dest {
                                    Variable::Named(_) => todo!(),
                                    Variable::Unnamed(dest) => result.push(LIRInstruction::Store(dest).into()),
                                }
                            }
                            ReferenceAccess::Member(dest_variable, dest_member) => {
                                push_variable(dest_variable, result);
                                push_variable(value, result);
                                result.push(LIRInstruction::AssignMember(dest_member).into());
                            }
                        },
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
                    if let Some(value) = value {
                        push_variable(value, result);
                    }
                    result.push(LIRInstruction::Return.into());
                }
                HIRStatement::Continue(_label) => {
                    todo!()
                }
                HIRStatement::Break(_label) => {
                    todo!()
                }
            }
            Ok(())
        }
        for statement in statements {
            convert(statement, result, sequence)?;
        }
        Ok(())
    }
    let mut result = Vec::new();
    convert_list(statements, &mut result, &mut 0)?;
    Ok(result)
}
