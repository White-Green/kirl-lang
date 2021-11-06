use std::convert::TryInto;
use std::error::Error;
use std::fmt::{Display, Formatter};

use kirl_common::dec::Decimal128;
use kirl_common::typing::{HIRType, LIRType, LIRTypeConvertError};
use uuid::Uuid;

use kirl_semantic_analyzer::{HIRExpression, HIRStatement, Immediate, ReferenceAccess, Variable};

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

pub fn hir_to_lir(statements: Vec<HIRStatement<(Uuid, HIRType)>>, argument_count: usize) -> Result<LIRStatementList, LIRStatementListConvertError> {
    fn convert_list(statements: impl IntoIterator<Item = HIRStatement<(Uuid, HIRType)>>, result: &mut Vec<LIRStatement>, sequence: &mut usize, loop_labels: &mut Vec<String>) -> Result<(), LIRStatementListConvertError> {
        fn convert(statement: HIRStatement<(Uuid, HIRType)>, result: &mut Vec<LIRStatement>, sequence: &mut usize, loop_labels: &mut Vec<String>) -> Result<(), LIRStatementListConvertError> {
            fn push_variable(variable: Variable<(Uuid, HIRType)>, result: &mut Vec<LIRStatement>) {
                match variable {
                    Variable::Named(_, _, (id, _)) => result.push(LIRInstruction::LoadNamedValue(id).into()),
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
                                Variable::Named(_, _, (id, _)) => id,
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
                                        Variable::Named(_, _, _) => todo!(),
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
                            result.push(LIRInstruction::ConstructArray(len).into());
                        }
                    }
                    result.push(LIRInstruction::Store(variable_id).into());
                }
                HIRStatement::Unreachable => {}
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
