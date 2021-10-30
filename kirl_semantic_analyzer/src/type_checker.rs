use std::collections::{BTreeMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::{Deref, Range};

use kirl_common::typing::HIRType;
use uuid::Uuid;

use kirl_parser::CharacterPosition;

use crate::name_resolver::ResolvedItems;
use crate::syntax_tree_to_hir::SearchPaths;
use crate::{HIRExpression, HIRStatement, Immediate, ReferenceAccess, Variable};

#[derive(Debug)]
pub enum DecisionTypeError {
    UnImplementedFeature(&'static str),
    ConflictUuid(Uuid),
    TypeMismatched { expected: HIRType, actual: HIRType },
    NamedReferenceIsNotUnique { position: Range<CharacterPosition>, found: ResolvedItems },
}

impl Display for DecisionTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DecisionTypeError::UnImplementedFeature(feature) => write!(f, "A feature {:?} is not implemented yet.", feature),
            DecisionTypeError::ConflictUuid(id) => write!(f, "Reference id {} is conflicted.", id),
            DecisionTypeError::TypeMismatched { expected, actual } => write!(f, "Expected type {} but found {}.", expected.to_string(), actual.to_string()),
            DecisionTypeError::NamedReferenceIsNotUnique { position, found: ResolvedItems(SearchPaths(paths), candidates) } => {
                let (path_last, path_other) = paths.split_last().expect("少なくともコードに書いた名前があるのでパスの候補は一つ以上あるはず");
                let paths = {
                    let mut paths = path_other
                        .iter()
                        .map(|path| format!("\"{}\", ", path.join("::")))
                        .reduce(|mut a, b| {
                            a.push_str(&b);
                            a
                        })
                        .map(|mut paths| {
                            paths.push_str("or ");
                            paths
                        })
                        .unwrap_or_default();
                    paths.push('"');
                    paths.push_str(&path_last.join("::"));
                    paths.push('"');
                    paths
                };
                if let Some((last, other)) = candidates.split_last() {
                    let mut candidates = other
                        .iter()
                        .map(|(path, _, ty)| format!("{}:{}, ", path.join("::"), ty.to_string()))
                        .reduce(|mut a, b| {
                            a.push_str(&b);
                            a
                        })
                        .map(|mut s| {
                            s.push_str("or ");
                            s
                        })
                        .unwrap_or_default();
                    candidates.push_str(&format!("{}:{}", last.0.join("::"), last.2.to_string()));
                    write!(f, "A named reference {} at {} - {} has multiple candidates {}.", paths, position.start, position.end, candidates)
                } else {
                    write!(f, "A named reference {} at {} - {} is not found.", paths, position.start, position.end)
                }
            }
        }
    }
}

impl Error for DecisionTypeError {}

type DecisionTypeResult<T> = Result<T, DecisionTypeError>;

pub fn decision_type(mut statements: Vec<HIRStatement<ResolvedItems>>, argument_types: Vec<HIRType>, return_type: HIRType) -> DecisionTypeResult<Vec<HIRStatement<(Uuid, HIRType)>>> {
    #[derive(Debug)]
    enum Reachable {
        Reachable,
        Unreachable,
    }
    fn decision_type_inner(statements: &mut [HIRStatement<ResolvedItems>], types: &mut Vec<HIRType>, return_type: &HIRType) -> DecisionTypeResult<Reachable> {
        fn decision_type_one(stmt: &mut HIRStatement<ResolvedItems>, types: &mut Vec<HIRType>, return_type: &HIRType) -> DecisionTypeResult<Reachable> {
            match stmt {
                //TODO:loop式を導入するときはもうちょっと考えないとだめ
                HIRStatement::Binding { variable_id, variable_type, expression } => {
                    if types.len() <= *variable_id {
                        types.resize_with(*variable_id + 1, || HIRType::Infer);
                    }
                    let (result_type, reachable) = match expression {
                        HIRExpression::Immediate(value) => match value {
                            Immediate::Number(_) => {
                                let type_number = HIRType::Named { path: vec!["Number".to_string()], generics_arguments: Vec::new() };
                                if !variable_type.is_a(&type_number) {
                                    return Err(DecisionTypeError::TypeMismatched { expected: type_number, actual: variable_type.clone() });
                                }
                                (type_number, Reachable::Reachable)
                            }
                            Immediate::String(_) => {
                                let type_string = HIRType::Named { path: vec!["String".to_string()], generics_arguments: Vec::new() };
                                if !variable_type.is_a(&type_string) {
                                    return Err(DecisionTypeError::TypeMismatched { expected: type_string, actual: variable_type.clone() });
                                }
                                (type_string, Reachable::Reachable)
                            }
                        },
                        HIRExpression::CallFunction { function, arguments: actual_arguments } => {
                            let (position, generics_arguments, ResolvedItems(paths, function)) = match function {
                                Variable::Named(position, generics_arguments, candidates) => (position.clone(), generics_arguments.as_slice(), candidates),
                                Variable::Unnamed(_) => {
                                    return Err(DecisionTypeError::UnImplementedFeature("Call function referenced by variable"));
                                }
                            };
                            take_mut::take(function, |function| {
                                function
                                    .into_iter()
                                    .filter_map(|(path, id, ty)| {
                                        let ty = match ty {
                                            ty @ HIRType::Function { .. } => ty,
                                            _ => return None,
                                        };
                                        let ty = ty.apply_generics_type_argument(generics_arguments)?;
                                        match &ty {
                                            HIRType::Function { arguments: formal_arguments, .. } => {
                                                if !(formal_arguments.len() == actual_arguments.len()
                                                    && actual_arguments.iter().zip(formal_arguments).all(|(actual, formal)| match actual {
                                                        Variable::Named(_, _, ResolvedItems(_, candidates)) => candidates.iter().any(|(_, _, ty)| ty.is_a(formal)),
                                                        Variable::Unnamed(id) => types[*id].is_a(formal),
                                                    }))
                                                {
                                                    return None;
                                                }
                                            }
                                            _ => unreachable!(),
                                        }
                                        Some((path, id, ty))
                                    })
                                    .collect()
                            });
                            if function.len() == 1 {
                                let (_, _, function_type) = function.last().unwrap();
                                if let HIRType::Function { arguments, result } = function_type {
                                    for (formal, actual) in arguments.iter().zip(actual_arguments) {
                                        if let Variable::Named(position, _, ResolvedItems(paths, candidates)) = actual {
                                            candidates.retain(|(_, _, ty)| ty.is_a(formal));
                                            if candidates.len() != 1 {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                    position: position.clone(),
                                                    found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                                });
                                            }
                                        }
                                    }
                                    (result.deref().clone(), Reachable::Reachable)
                                } else {
                                    unreachable!("retainでチェックしてるのであんりーちゃぶる")
                                }
                            } else {
                                return Err(DecisionTypeError::NamedReferenceIsNotUnique { position, found: ResolvedItems(mem::take(paths), mem::take(function)) });
                            }
                        }
                        HIRExpression::AccessVariable(variable) => match variable {
                            Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                if let [(_, _, ty)] = candidates.as_slice() {
                                    (ty.clone(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                        position: position.clone(),
                                        found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                    });
                                }
                            }
                            Variable::Unnamed(id) => (types[*id].clone(), Reachable::Reachable),
                        },
                        HIRExpression::AccessTupleItem { variable, index } => match variable {
                            Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                candidates.retain(|(_, _, ty)| ty.has_tuple_item(*index));
                                if let [(_, _, ty)] = candidates.as_slice() {
                                    (ty.tuple_item_type(*index).unwrap().into_owned(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                        position: position.clone(),
                                        found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                    });
                                }
                            }
                            Variable::Unnamed(id) => {
                                if let Some(ty) = types[*id].tuple_item_type(*index) {
                                    (ty.into_owned(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::TypeMismatched {
                                        expected: HIRType::Tuple(vec![HIRType::Infer; *index + 1]),
                                        actual: types[*id].clone(),
                                    });
                                }
                            }
                        },
                        HIRExpression::AccessMember { variable, member } => match variable {
                            Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                candidates.retain(|(_, _, ty)| ty.has_member(member));
                                if let [(_, _, ty)] = candidates.as_slice() {
                                    (ty.member_type(member).unwrap().into_owned(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                        position: position.clone(),
                                        found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                    });
                                }
                            }
                            Variable::Unnamed(id) => {
                                if let Some(ty) = types[*id].member_type(member) {
                                    (ty.into_owned(), Reachable::Reachable)
                                } else {
                                    return Err(DecisionTypeError::TypeMismatched {
                                        expected: HIRType::AnonymousStruct([(member.clone(), HIRType::Infer)].into_iter().collect()),
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
                            let bool_type = HIRType::Named { path: vec!["Bool".to_string()], generics_arguments: Vec::new() };
                            match condition {
                                Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                    candidates.retain(|(_, _, ty)| ty.is_a(&bool_type));
                                    if candidates.len() != 1 {
                                        return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                            position: position.clone(),
                                            found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                        });
                                    }
                                }
                                Variable::Unnamed(id) => {
                                    if !types[*id].is_a(&bool_type) {
                                        return Err(DecisionTypeError::TypeMismatched { expected: bool_type, actual: types[*id].clone() });
                                    }
                                }
                            }
                            let mut unreachable = true;
                            let then_type = match decision_type_inner(then_statements, types, return_type)? {
                                Reachable::Unreachable => HIRType::Unreachable,
                                Reachable::Reachable => {
                                    unreachable = false;
                                    match then_expr {
                                        Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                            if candidates.len() == 1 {
                                                candidates.first().unwrap().2.clone()
                                            } else {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                    position: position.clone(),
                                                    found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                                });
                                            }
                                        }
                                        Variable::Unnamed(id) => types[*id].clone(),
                                    }
                                }
                            };
                            let other_type = match decision_type_inner(other_statements, types, return_type)? {
                                Reachable::Unreachable => HIRType::Unreachable,
                                Reachable::Reachable => {
                                    unreachable = false;
                                    match other_expr {
                                        Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                            if candidates.len() == 1 {
                                                candidates.first().unwrap().2.clone()
                                            } else {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                    position: position.clone(),
                                                    found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                                });
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
                        HIRExpression::IfLet {
                            condition_binding,
                            pattern_type,
                            condition,
                            then: (then_statements, then_expr),
                            other: (other_statements, other_expr),
                        } => {
                            let (possibility_assign, is_a, mut condition_type) = match condition {
                                Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                    candidates.retain(|(_, _, ty)| ty.is_a(pattern_type));
                                    if candidates.len() != 1 {
                                        return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                            position: position.clone(),
                                            found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                        });
                                    }
                                    (true, true, candidates[0].2.clone())
                                }
                                Variable::Unnamed(id) => (types[*id].possibility_assignable_to(pattern_type), types[*id].is_a(pattern_type), pattern_type.intersect_to(&types[*id])),
                            };
                            assert!(!is_a || possibility_assign); // is_a=trueなのにpossibility_assign=falseは無いよ
                            if is_a {
                                *other_statements = vec![HIRStatement::Unreachable];
                            }
                            if !possibility_assign {
                                condition_type = condition_type.infer_temporary();
                                *then_statements = vec![HIRStatement::Unreachable];
                            }
                            types[*condition_binding] = condition_type.clone();
                            *pattern_type = condition_type;
                            let mut unreachable = true;
                            let then_type = match decision_type_inner(then_statements, types, return_type)? {
                                Reachable::Unreachable => HIRType::Unreachable,
                                Reachable::Reachable => {
                                    unreachable = false;
                                    match then_expr {
                                        Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                            if candidates.len() == 1 {
                                                candidates.first().unwrap().2.clone()
                                            } else {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                    position: position.clone(),
                                                    found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                                });
                                            }
                                        }
                                        Variable::Unnamed(id) => types[*id].clone(),
                                    }
                                }
                            };
                            let other_type = match decision_type_inner(other_statements, types, return_type)? {
                                Reachable::Unreachable => HIRType::Unreachable,
                                Reachable::Reachable => {
                                    unreachable = false;
                                    match other_expr {
                                        Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                            if candidates.len() == 1 {
                                                candidates.first().unwrap().2.clone()
                                            } else {
                                                return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                    position: position.clone(),
                                                    found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                                });
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
                        HIRExpression::Loop(inner) => {
                            decision_type_inner(inner, types, return_type)?;
                            (HIRType::Tuple(Vec::new()), Reachable::Reachable)
                        }
                        HIRExpression::Assign { variable, value } => {
                            let value_type = match value {
                                Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                    if let [(_, _, ty)] = candidates.as_slice() {
                                        ty
                                    } else {
                                        return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                            position: position.clone(),
                                            found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                        });
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
                                ReferenceAccess::TupleItem(variable, index) => {
                                    if let Variable::Unnamed(variable) = variable {
                                        let variable_type = &types[*variable];
                                        if let Some(ty) = variable_type.tuple_item_type(*index) {
                                            if value_type.is_a(&ty) {
                                                (ty.into_owned(), Reachable::Reachable)
                                            } else {
                                                return Err(DecisionTypeError::TypeMismatched { expected: ty.into_owned(), actual: value_type.clone() });
                                            }
                                        } else {
                                            return Err(DecisionTypeError::TypeMismatched {
                                                expected: HIRType::Tuple([HIRType::Infer].into_iter().cycle().take(*index).chain([value_type.clone()]).collect()),
                                                actual: variable_type.clone(),
                                            });
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
                                                expected: HIRType::AnonymousStruct([(member.clone(), value_type.clone())].into_iter().collect()),
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
                                    Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                        if let [(_, _, ty)] = candidates.as_slice() {
                                            ty.clone()
                                        } else {
                                            return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                position: position.clone(),
                                                found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                            });
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
                                    Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                        if let [(_, _, ty)] = candidates.as_slice() {
                                            items_type.push(ty.clone());
                                        } else {
                                            return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                position: position.clone(),
                                                found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                            });
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
                                    Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                                        if let [(_, _, ty)] = candidates.as_slice() {
                                            items_type.push(ty.clone());
                                        } else {
                                            return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                                position: position.clone(),
                                                found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                            });
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
                    if !result_type.is_a(variable_type) {
                        return Err(DecisionTypeError::TypeMismatched { expected: variable_type.clone(), actual: result_type });
                    }
                    *variable_type = variable_type.intersect_to(&result_type);
                    types[*variable_id] = variable_type.clone();
                    Ok(reachable)
                }
                HIRStatement::Unreachable => Ok(Reachable::Unreachable),
                HIRStatement::Return(return_value) => {
                    match return_value {
                        Variable::Named(position, _, ResolvedItems(paths, candidates)) => {
                            candidates.retain(|(_, _, ty)| ty.is_a(return_type));
                            if candidates.len() != 1 {
                                return Err(DecisionTypeError::NamedReferenceIsNotUnique {
                                    position: position.clone(),
                                    found: ResolvedItems(mem::take(paths), mem::take(candidates)),
                                });
                            }
                        }
                        Variable::Unnamed(id) => {
                            if !types[*id].is_a(return_type) {
                                return Err(DecisionTypeError::TypeMismatched { expected: return_type.clone(), actual: types[*id].clone() });
                            }
                        }
                    }
                    Ok(Reachable::Unreachable)
                }
                HIRStatement::Continue(_) => Ok(Reachable::Unreachable),
                HIRStatement::Break(_) => Ok(Reachable::Unreachable),
            }
        }
        for stmt in statements {
            if let Reachable::Unreachable = decision_type_one(stmt, types, return_type)? {
                return Ok(Reachable::Unreachable);
            }
        }
        Ok(Reachable::Reachable)
    }

    fn into_one_candidate(statements: Vec<HIRStatement<ResolvedItems>>) -> Vec<HIRStatement<(Uuid, HIRType)>> {
        fn into_one(variable: Variable<ResolvedItems>) -> Variable<(Uuid, HIRType)> {
            match variable {
                Variable::Named(position, types, ResolvedItems(_, mut candidates)) => {
                    assert_eq!(candidates.len(), 1, "前半で候補はひとつに絞られているはず");
                    let (_, id, ty) = candidates.pop().unwrap();
                    Variable::Named(position, types, (id, ty))
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
                        HIRExpression::AccessTupleItem { variable, index } => HIRExpression::AccessTupleItem { variable: into_one(variable), index },
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
                                ReferenceAccess::TupleItem(variable, index) => ReferenceAccess::TupleItem(into_one(variable), index),
                                ReferenceAccess::Member(variable, member) => ReferenceAccess::Member(into_one(variable), member),
                            },
                            value: into_one(value),
                        },
                        HIRExpression::ConstructStruct(items) => HIRExpression::ConstructStruct(items.into_iter().map(|(k, v)| (k, into_one(v))).collect()),
                        HIRExpression::ConstructTuple(items) => HIRExpression::ConstructTuple(items.into_iter().map(into_one).collect()),
                        HIRExpression::ConstructArray(items) => HIRExpression::ConstructArray(items.into_iter().map(into_one).collect()),
                    },
                },
                HIRStatement::Unreachable => HIRStatement::Unreachable,
                HIRStatement::Return(value) => HIRStatement::Return(into_one(value)),
                HIRStatement::Continue(label) => HIRStatement::Continue(label),
                HIRStatement::Break(label) => HIRStatement::Break(label),
            })
            .collect()
    }
    let mut types = argument_types;
    decision_type_inner(&mut statements, &mut types, &return_type)?;
    Ok(into_one_candidate(statements))
}

pub fn used_functions(statements: &[HIRStatement<(Uuid, HIRType)>]) -> HashSet<Uuid> {
    let mut result = HashSet::new();
    fn inner(statements: &[HIRStatement<(Uuid, HIRType)>], result: &mut HashSet<Uuid>) {
        fn add_used_variable(variable: &Variable<(Uuid, HIRType)>, result: &mut HashSet<Uuid>) {
            match variable {
                Variable::Named(_, _, (id, _)) => {
                    result.insert(*id);
                }
                Variable::Unnamed(_) => {}
            }
        }
        for statement in statements {
            match statement {
                HIRStatement::Binding { expression, .. } => match expression {
                    HIRExpression::Immediate(_) => {}
                    HIRExpression::CallFunction { function, arguments } => {
                        add_used_variable(function, result);
                        for argument in arguments {
                            add_used_variable(argument, result);
                        }
                    }
                    HIRExpression::AccessVariable(variable) => add_used_variable(variable, result),
                    HIRExpression::AccessTupleItem { variable, .. } => add_used_variable(variable, result),
                    HIRExpression::AccessMember { variable, .. } => add_used_variable(variable, result),
                    HIRExpression::If {
                        condition,
                        then: (then_statements, then_result),
                        other: (other_statements, other_result),
                    } => {
                        add_used_variable(condition, result);
                        inner(then_statements, result);
                        add_used_variable(then_result, result);
                        inner(other_statements, result);
                        add_used_variable(other_result, result);
                    }
                    HIRExpression::IfLet {
                        condition,
                        then: (then_statements, then_result),
                        other: (other_statements, other_result),
                        ..
                    } => {
                        add_used_variable(condition, result);
                        inner(then_statements, result);
                        add_used_variable(then_result, result);
                        inner(other_statements, result);
                        add_used_variable(other_result, result);
                    }
                    HIRExpression::Loop(statements) => inner(statements, result),
                    HIRExpression::Assign { variable, value } => {
                        match variable {
                            ReferenceAccess::Variable(variable) => add_used_variable(variable, result),
                            ReferenceAccess::TupleItem(variable, _) => add_used_variable(variable, result),
                            ReferenceAccess::Member(variable, _) => add_used_variable(variable, result),
                        }
                        add_used_variable(value, result);
                    }
                    HIRExpression::ConstructStruct(members) => {
                        for member_value in members.values() {
                            add_used_variable(member_value, result);
                        }
                    }
                    HIRExpression::ConstructTuple(items) => {
                        for item in items {
                            add_used_variable(item, result);
                        }
                    }
                    HIRExpression::ConstructArray(items) => {
                        for item in items {
                            add_used_variable(item, result);
                        }
                    }
                },
                HIRStatement::Unreachable => {}
                HIRStatement::Return(variable) => add_used_variable(variable, result),
                HIRStatement::Continue(_) => {}
                HIRStatement::Break(_) => {}
            }
        }
    }
    inner(statements, &mut result);
    result
}
