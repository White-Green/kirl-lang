use core::array;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

use uuid::Uuid;

use crate::{HIRExpression, HIRStatement, HIRType, Immediate, ReferenceAccess, Variable};

#[derive(Debug)]
pub enum DecisionTypeError {
    UnImplementedFeature(&'static str),
    ConflictUuid(Uuid),
    TypeMismatched { expected: HIRType, actual: HIRType },
    NamedReferenceIsNotUnique(Vec<Uuid>),
}

impl Display for DecisionTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DecisionTypeError::UnImplementedFeature(feature) => write!(f, "feature {:?} is not implemented yet.", feature),
            DecisionTypeError::ConflictUuid(id) => write!(f, "reference id {} is conflicted.", id),
            DecisionTypeError::TypeMismatched { expected, actual } => write!(f, "expected type {} but found {}", expected.to_string(), actual.to_string()),
            DecisionTypeError::NamedReferenceIsNotUnique(list) => write!(f, "named reference is not unique(found {} references)", list.len()),
        }
    }
}

impl Error for DecisionTypeError {}

type DecisionTypeResult<T> = Result<T, DecisionTypeError>;

pub fn decision_type(mut statements: Vec<HIRStatement<Vec<(Uuid, HIRType)>>>) -> DecisionTypeResult<Vec<HIRStatement<(Uuid, HIRType)>>> {
    enum Reachable {
        Reachable,
        Unreachable,
    }
    fn decision_type_inner(statements: &mut [HIRStatement<Vec<(Uuid, HIRType)>>], types: &mut Vec<HIRType>) -> DecisionTypeResult<Reachable> {
        fn decision_type_one(stmt: &mut HIRStatement<Vec<(Uuid, HIRType)>>, types: &mut Vec<HIRType>) -> DecisionTypeResult<Reachable> {
            match stmt {
                //TODO:loop式を導入するときはもうちょっと考えないとだめ
                HIRStatement::Binding { variable_id, variable_type, expression } => {
                    types.resize_with(*variable_id + 1, || HIRType::Infer);
                    let (result_type, reachable) = match expression {
                        HIRExpression::Immediate(value) => match value {
                            Immediate::Integer(_) => return Err(DecisionTypeError::UnImplementedFeature("Integer immediate")),
                            Immediate::Float(_) => return Err(DecisionTypeError::UnImplementedFeature("Float immediate")),
                            Immediate::String(_value) => {
                                let type_string = HIRType::Named { path: vec!["String".to_string()], generics_arguments: Vec::new() };
                                if !variable_type.is_a(&type_string) {
                                    return Err(DecisionTypeError::TypeMismatched { expected: type_string, actual: variable_type.clone() });
                                }
                                (type_string, Reachable::Reachable)
                            }
                        },
                        HIRExpression::CallFunction { function, arguments: actual_arguments } => {
                            let function = match function {
                                Variable::Named(candidates) => candidates,
                                Variable::Unnamed(_) => {
                                    return Err(DecisionTypeError::UnImplementedFeature("Call function referenced by variable"));
                                }
                            };
                            function.retain(|(_, ty)| match ty {
                                HIRType::Function { arguments: formal_arguments, .. } => {
                                    formal_arguments.len() == actual_arguments.len()
                                        && actual_arguments.iter().zip(formal_arguments).all(|(actual, formal)| match actual {
                                            Variable::Named(candidates) => candidates.iter().any(|(_, ty)| ty.is_a(formal)),
                                            Variable::Unnamed(id) => types[*id].is_a(formal),
                                        })
                                }
                                _ => false,
                            });
                            if function.len() == 1 {
                                let (_, function_type) = function.last().unwrap();
                                if let HIRType::Function { arguments, result } = function_type {
                                    for (formal, actual) in arguments.iter().zip(actual_arguments) {
                                        if let Variable::Named(candidates) = actual {
                                            candidates.retain(|(_, ty)| ty.is_a(formal));
                                            if candidates.len() != 1 {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                            }
                                        }
                                    }
                                    (result.deref().clone(), Reachable::Reachable)
                                } else {
                                    unreachable!("retainでチェックしてるのであんりーちゃぶる")
                                }
                            } else {
                                return Err(DecisionTypeError::NamedReferenceIsNotUnique(function.iter().map(|(id, _)| *id).collect()));
                            }
                        }
                        HIRExpression::AccessVariable(variable) => match variable {
                            Variable::Named(candidates) => {
                                if let [(_, ty)] = candidates.as_slice() {
                                    (ty.clone(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                }
                            }
                            Variable::Unnamed(id) => (types[*id].clone(), Reachable::Reachable),
                        },
                        HIRExpression::AccessMember { variable, member } => match variable {
                            Variable::Named(candidates) => {
                                candidates.retain(|(_, ty)| ty.has_member(member));
                                if let [(_, ty)] = candidates.as_slice() {
                                    (ty.member_type(member).unwrap().into_owned(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                }
                            }
                            Variable::Unnamed(id) => {
                                if let Some(ty) = types[*id].member_type(member) {
                                    (ty.into_owned(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::TypeMismatched {
                                        expected: HIRType::AnonymousStruct(array::IntoIter::new([(member.clone(), HIRType::Infer)]).collect()),
                                        actual: types[*id].clone(),
                                    });
                                }
                            }
                        },
                        HIRExpression::If {
                            condition,
                            then: (then_statements, then_expr),
                            other: (other_statements, other_expr),
                        } => {
                            let bool_type = HIRType::Named { path: vec!["bool".to_string()], generics_arguments: Vec::new() };
                            match condition {
                                Variable::Named(candidates) => {
                                    candidates.retain(|(_, ty)| ty.is_a(&bool_type));
                                    if candidates.len() != 1 {
                                        return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                    }
                                }
                                Variable::Unnamed(id) => {
                                    if !types[*id].is_a(&bool_type) {
                                        return Err(DecisionTypeError::TypeMismatched { expected: bool_type, actual: types[*id].clone() });
                                    }
                                }
                            }
                            let mut unreachable = true;
                            let then_type = match decision_type_inner(then_statements, types)? {
                                Reachable::Unreachable => HIRType::Unreachable,
                                Reachable::Reachable => {
                                    unreachable = false;
                                    match then_expr {
                                        Variable::Named(candidates) => {
                                            if candidates.len() == 1 {
                                                candidates.first().unwrap().1.clone()
                                            } else {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                            }
                                        }
                                        Variable::Unnamed(id) => types[*id].clone(),
                                    }
                                }
                            };
                            let other_type = match decision_type_inner(other_statements, types)? {
                                Reachable::Unreachable => HIRType::Unreachable,
                                Reachable::Reachable => {
                                    unreachable = false;
                                    match other_expr {
                                        Variable::Named(candidates) => {
                                            if candidates.len() == 1 {
                                                candidates.first().unwrap().1.clone()
                                            } else {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                            }
                                        }
                                        Variable::Unnamed(id) => types[*id].clone(),
                                    }
                                }
                            };
                            let mut result_type = HIRType::Or(vec![then_type, other_type]);
                            result_type.normalize();
                            (result_type, if unreachable { Reachable::Unreachable } else { Reachable::Reachable })
                        }
                        HIRExpression::IfLet { .. } => return Err(DecisionTypeError::UnImplementedFeature("type checking for if-var expression")),
                        HIRExpression::Loop(inner) => {
                            decision_type_inner(inner, types)?;
                            (HIRType::None, Reachable::Reachable)
                        }
                        HIRExpression::Assign { variable, value } => {
                            let value_type = match value {
                                Variable::Named(candidates) => {
                                    if let [(_, ty)] = candidates.as_slice() {
                                        ty
                                    } else {
                                        return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                    }
                                }
                                Variable::Unnamed(id) => &types[*id],
                            };
                            match variable {
                                ReferenceAccess::Variable(variable) => {
                                    if let Variable::Unnamed(variable) = variable {
                                        if value_type.is_a(&types[*variable]) {
                                            (types[*variable].clone(), Reachable::Reachable)
                                        } else {
                                            return Err(DecisionTypeError::TypeMismatched { expected: types[*variable].clone(), actual: value_type.clone() });
                                        }
                                    } else {
                                        return Err(DecisionTypeError::UnImplementedFeature("assign to un-local variable"));
                                    }
                                }
                                ReferenceAccess::Member(variable, member) => {
                                    if let Variable::Unnamed(variable) = variable {
                                        let variable_type = &types[*variable];
                                        if let Some(ty) = variable_type.member_type(member) {
                                            if value_type.is_a(&ty) {
                                                (ty.into_owned(), Reachable::Reachable)
                                            } else {
                                                return Err(DecisionTypeError::TypeMismatched { expected: ty.into_owned(), actual: value_type.clone() });
                                            }
                                        } else {
                                            return Err(DecisionTypeError::TypeMismatched {
                                                expected: HIRType::AnonymousStruct(array::IntoIter::new([(member.clone(), value_type.clone())]).collect()),
                                                actual: variable_type.clone(),
                                            });
                                        }
                                    } else {
                                        return Err(DecisionTypeError::UnImplementedFeature("assign to un-local variable"));
                                    }
                                }
                            }
                        }
                        HIRExpression::ConstructStruct(items) => {
                            let mut items_type = BTreeMap::new();
                            for (member, variable) in items {
                                let ty = match variable {
                                    Variable::Named(candidates) => {
                                        if let [(_, ty)] = candidates.as_slice() {
                                            ty.clone()
                                        } else {
                                            return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                        }
                                    }
                                    Variable::Unnamed(id) => types[*id].clone(),
                                };
                                items_type.insert(member.clone(), ty);
                            }
                            (HIRType::AnonymousStruct(items_type), Reachable::Reachable)
                        }
                        HIRExpression::ConstructTuple(items) => {
                            let mut items_type = Vec::with_capacity(items.len());
                            for variable in items {
                                match variable {
                                    Variable::Named(candidates) => {
                                        if let [(_, ty)] = candidates.as_slice() {
                                            items_type.push(ty.clone());
                                        } else {
                                            return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                        }
                                    }
                                    Variable::Unnamed(id) => {
                                        items_type.push(types[*id].clone());
                                    }
                                }
                            }
                            let mut result_type = HIRType::Tuple(items_type);
                            result_type.normalize();
                            (result_type, Reachable::Reachable)
                        }
                        HIRExpression::ConstructArray(items) => {
                            let mut items_type = Vec::with_capacity(items.len());
                            for variable in items {
                                match variable {
                                    Variable::Named(candidates) => {
                                        if let [(_, ty)] = candidates.as_slice() {
                                            items_type.push(ty.clone());
                                        } else {
                                            return Err(DecisionTypeError::NamedReferenceIsNotUnique(candidates.iter().map(|(id, _)| *id).collect()));
                                        }
                                    }
                                    Variable::Unnamed(id) => {
                                        items_type.push(types[*id].clone());
                                    }
                                }
                            }
                            let mut result_type = HIRType::Array(Box::new(HIRType::Or(items_type)));
                            result_type.normalize();
                            (result_type, Reachable::Reachable)
                        }
                    };
                    *variable_type = result_type.clone();
                    types[*variable_id] = result_type;
                    Ok(reachable)
                }
                HIRStatement::Return(_) => Ok(Reachable::Unreachable),
                HIRStatement::Continue(_) => Ok(Reachable::Unreachable),
                HIRStatement::Break(_) => Ok(Reachable::Unreachable),
            }
        }
        for stmt in statements {
            decision_type_one(stmt, types)?;
        }
        Ok(Reachable::Reachable)
    }

    fn into_one_candidate(statements: Vec<HIRStatement<Vec<(Uuid, HIRType)>>>) -> Vec<HIRStatement<(Uuid, HIRType)>> {
        fn into_one(variable: Variable<Vec<(Uuid, HIRType)>>) -> Variable<(Uuid, HIRType)> {
            match variable {
                Variable::Named(mut candidates) => {
                    assert_eq!(candidates.len(), 1, "前半で候補はひとつに絞られているはず");
                    Variable::Named(candidates.pop().unwrap())
                }
                Variable::Unnamed(id) => Variable::Unnamed(id),
            }
        }
        statements
            .into_iter()
            .map(|statement| match statement {
                HIRStatement::Binding { variable_id, variable_type, expression } => HIRStatement::Binding {
                    variable_id,
                    variable_type,
                    expression: match expression {
                        HIRExpression::Immediate(value) => HIRExpression::Immediate(value),
                        HIRExpression::CallFunction { function, arguments } => HIRExpression::CallFunction {
                            function: into_one(function),
                            arguments: arguments.into_iter().map(into_one).collect(),
                        },
                        HIRExpression::AccessVariable(variable) => HIRExpression::AccessVariable(into_one(variable)),
                        HIRExpression::AccessMember { variable, member } => HIRExpression::AccessMember { variable: into_one(variable), member },
                        HIRExpression::If {
                            condition,
                            then: (then_statements, then_expr),
                            other: (other_statements, other_expr),
                        } => HIRExpression::If {
                            condition: into_one(condition),
                            then: (into_one_candidate(then_statements), into_one(then_expr)),
                            other: (into_one_candidate(other_statements), into_one(other_expr)),
                        },
                        HIRExpression::IfLet {
                            condition_binding,
                            pattern_type,
                            condition,
                            then: (then_statements, then_expr),
                            other: (other_statements, other_expr),
                        } => HIRExpression::IfLet {
                            condition_binding,
                            pattern_type,
                            condition: into_one(condition),
                            then: (into_one_candidate(then_statements), into_one(then_expr)),
                            other: (into_one_candidate(other_statements), into_one(other_expr)),
                        },
                        HIRExpression::Loop(stmt) => HIRExpression::Loop(into_one_candidate(stmt)),
                        HIRExpression::Assign { variable, value } => HIRExpression::Assign {
                            variable: match variable {
                                ReferenceAccess::Variable(variable) => ReferenceAccess::Variable(into_one(variable)),
                                ReferenceAccess::Member(variable, member) => ReferenceAccess::Member(into_one(variable), member),
                            },
                            value: into_one(value),
                        },
                        HIRExpression::ConstructStruct(items) => HIRExpression::ConstructStruct(items.into_iter().map(|(k, v)| (k, into_one(v))).collect()),
                        HIRExpression::ConstructTuple(items) => HIRExpression::ConstructTuple(items.into_iter().map(into_one).collect()),
                        HIRExpression::ConstructArray(items) => HIRExpression::ConstructArray(items.into_iter().map(into_one).collect()),
                    },
                },
                HIRStatement::Return(value) => HIRStatement::Return(value.map(into_one)),
                HIRStatement::Continue(label) => HIRStatement::Continue(label),
                HIRStatement::Break(label) => HIRStatement::Break(label),
            })
            .collect()
    }
    let mut types = Vec::new();
    decision_type_inner(&mut statements, &mut types)?;
    Ok(into_one_candidate(statements))
}
