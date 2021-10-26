use std::collections::{BTreeMap, HashMap};

use uuid::Uuid;

use crate::syntax_tree_to_hir::SearchPaths;
use crate::{HIRExpression, HIRStatement, HIRType, ReferenceAccess, Variable};

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct ResolvedItems(pub(crate) SearchPaths, pub(crate) Vec<(Vec<String>, Uuid, HIRType)>);

pub trait KirlNameResolver {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)>;
}

impl<R: ?Sized + KirlNameResolver> KirlNameResolver for &mut R {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        (*self).resolve(full_path)
    }
}

impl<R1> KirlNameResolver for (R1,)
where
    R1: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        let (r1,) = self;
        r1.resolve(full_path)
    }
}

impl<R1, R2> KirlNameResolver for (R1, R2)
where
    R1: KirlNameResolver,
    R2: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        let (r1, r2) = self;
        let mut result = r1.resolve(full_path);
        result.extend(r2.resolve(full_path));
        result
    }
}

impl<R1, R2, R3> KirlNameResolver for (R1, R2, R3)
where
    R1: KirlNameResolver,
    R2: KirlNameResolver,
    R3: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        let (r1, r2, r3) = self;
        let mut result = r1.resolve(full_path);
        result.extend(r2.resolve(full_path));
        result.extend(r3.resolve(full_path));
        result
    }
}

impl<R1, R2, R3, R4> KirlNameResolver for (R1, R2, R3, R4)
where
    R1: KirlNameResolver,
    R2: KirlNameResolver,
    R3: KirlNameResolver,
    R4: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        let (r1, r2, r3, r4) = self;
        let mut result = r1.resolve(full_path);
        result.extend(r2.resolve(full_path));
        result.extend(r3.resolve(full_path));
        result.extend(r4.resolve(full_path));
        result
    }
}

impl<R1, R2, R3, R4, R5> KirlNameResolver for (R1, R2, R3, R4, R5)
where
    R1: KirlNameResolver,
    R2: KirlNameResolver,
    R3: KirlNameResolver,
    R4: KirlNameResolver,
    R5: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        let (r1, r2, r3, r4, r5) = self;
        let mut result = r1.resolve(full_path);
        result.extend(r2.resolve(full_path));
        result.extend(r3.resolve(full_path));
        result.extend(r4.resolve(full_path));
        result.extend(r5.resolve(full_path));
        result
    }
}

impl<R: KirlNameResolver> KirlNameResolver for [R] {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        self.iter_mut().flat_map(|resolver| resolver.resolve(full_path)).collect()
    }
}

impl<R: KirlNameResolver> KirlNameResolver for HashMap<String, R> {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        full_path.first().and_then(|key| self.get_mut(key)).map(|resolver| resolver.resolve(&full_path[1..])).unwrap_or_default()
    }
}

impl<R: KirlNameResolver> KirlNameResolver for BTreeMap<String, R> {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        full_path.first().and_then(|key| self.get_mut(key)).map(|resolver| resolver.resolve(&full_path[1..])).unwrap_or_default()
    }
}

trait Resolvable {
    type ResolveResult;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult;
    fn all_reference(&self) -> Vec<&[String]>;
}

impl<T: Resolvable> Resolvable for Vec<T> {
    type ResolveResult = Vec<T::ResolveResult>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        self.into_iter().map(|v| v.resolve(resolver)).collect()
    }

    fn all_reference(&self) -> Vec<&[String]> {
        self.iter().flat_map(Resolvable::all_reference).collect()
    }
}

impl<T: Resolvable, U: Resolvable> Resolvable for (T, U) {
    type ResolveResult = (T::ResolveResult, U::ResolveResult);
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        let (t, u) = self;
        (t.resolve(resolver), u.resolve(resolver))
    }

    fn all_reference(&self) -> Vec<&[String]> {
        let (t, u) = self;
        let mut result = t.all_reference();
        result.extend(u.all_reference());
        result
    }
}

impl Resolvable for Variable<SearchPaths> {
    type ResolveResult = Variable<ResolvedItems>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            Variable::Named(range, paths) => {
                let reference = ResolvedItems(paths.clone(), paths.0.into_iter().flat_map(|path| [path.clone()].into_iter().cycle().zip(resolver.resolve(&path)).map(|(path, (id, ty))| (path, id, ty))).collect());
                Variable::Named(range, reference)
            }
            Variable::Unnamed(id) => Variable::Unnamed(id),
        }
    }

    fn all_reference(&self) -> Vec<&[String]> {
        match self {
            Variable::Named(_, SearchPaths(paths)) => paths.iter().map(Vec::as_slice).collect(),
            Variable::Unnamed(_) => Vec::new(),
        }
    }
}

impl Resolvable for ReferenceAccess<SearchPaths> {
    type ResolveResult = ReferenceAccess<ResolvedItems>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            ReferenceAccess::Variable(variable) => ReferenceAccess::Variable(variable.resolve(resolver)),
            ReferenceAccess::TupleItem(variable, index) => ReferenceAccess::TupleItem(variable.resolve(resolver), index),
            ReferenceAccess::Member(variable, member) => ReferenceAccess::Member(variable.resolve(resolver), member),
        }
    }

    fn all_reference(&self) -> Vec<&[String]> {
        match self {
            ReferenceAccess::Variable(variable) => variable.all_reference(),
            ReferenceAccess::TupleItem(variable, _) => variable.all_reference(),
            ReferenceAccess::Member(variable, _) => variable.all_reference(),
        }
    }
}

impl Resolvable for HIRExpression<SearchPaths> {
    type ResolveResult = HIRExpression<ResolvedItems>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            HIRExpression::Immediate(value) => HIRExpression::Immediate(value),
            HIRExpression::CallFunction { function, arguments } => HIRExpression::CallFunction {
                function: function.resolve(resolver),
                arguments: arguments.resolve(resolver),
            },
            HIRExpression::AccessVariable(variable) => HIRExpression::AccessVariable(variable.resolve(resolver)),
            HIRExpression::AccessTupleItem { variable, index } => HIRExpression::AccessTupleItem { variable: variable.resolve(resolver), index },
            HIRExpression::AccessMember { variable, member } => HIRExpression::AccessMember { variable: variable.resolve(resolver), member },
            HIRExpression::If { condition, then, other } => HIRExpression::If {
                condition: condition.resolve(resolver),
                then: then.resolve(resolver),
                other: other.resolve(resolver),
            },
            HIRExpression::IfLet { condition_binding, pattern_type, condition, then, other } => HIRExpression::IfLet {
                condition_binding,
                pattern_type,
                condition: condition.resolve(resolver),
                then: then.resolve(resolver),
                other: other.resolve(resolver),
            },
            HIRExpression::Loop(statements) => HIRExpression::Loop(statements.resolve(resolver)),
            HIRExpression::Assign { variable, value } => HIRExpression::Assign { variable: variable.resolve(resolver), value: value.resolve(resolver) },
            HIRExpression::ConstructStruct(members) => HIRExpression::ConstructStruct(members.into_iter().map(|(k, v)| (k, v.resolve(resolver))).collect()),
            HIRExpression::ConstructTuple(members) => HIRExpression::ConstructTuple(members.resolve(resolver)),
            HIRExpression::ConstructArray(members) => HIRExpression::ConstructArray(members.resolve(resolver)),
        }
    }

    fn all_reference(&self) -> Vec<&[String]> {
        match self {
            HIRExpression::Immediate(_) => Vec::new(),
            HIRExpression::CallFunction { function, arguments } => {
                let mut result = function.all_reference();
                result.extend(arguments.all_reference());
                result
            }
            HIRExpression::AccessVariable(variable) => variable.all_reference(),
            HIRExpression::AccessTupleItem { variable, .. } => variable.all_reference(),
            HIRExpression::AccessMember { variable, .. } => variable.all_reference(),
            HIRExpression::If { condition, then, other } => {
                let mut result = condition.all_reference();
                result.extend(then.all_reference());
                result.extend(other.all_reference());
                result
            }
            HIRExpression::IfLet { condition, then, other, .. } => {
                let mut result = condition.all_reference();
                result.extend(then.all_reference());
                result.extend(other.all_reference());
                result
            }
            HIRExpression::Loop(statements) => statements.all_reference(),
            HIRExpression::Assign { variable, value } => {
                let mut result = variable.all_reference();
                result.extend(value.all_reference());
                result
            }
            HIRExpression::ConstructStruct(members) => members.values().flat_map(Resolvable::all_reference).collect(),
            HIRExpression::ConstructTuple(items) => items.all_reference(),
            HIRExpression::ConstructArray(items) => items.all_reference(),
        }
    }
}

impl Resolvable for HIRStatement<SearchPaths> {
    type ResolveResult = HIRStatement<ResolvedItems>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            HIRStatement::Binding { variable_id, variable_type, expression } => HIRStatement::Binding { variable_id, variable_type, expression: expression.resolve(resolver) },
            HIRStatement::Unreachable => HIRStatement::Unreachable,
            HIRStatement::Return(value) => HIRStatement::Return(value.resolve(resolver)),
            HIRStatement::Continue(label) => HIRStatement::Continue(label),
            HIRStatement::Break(label) => HIRStatement::Break(label),
        }
    }

    fn all_reference(&self) -> Vec<&[String]> {
        match self {
            HIRStatement::Binding { expression, .. } => expression.all_reference(),
            HIRStatement::Unreachable => Vec::new(),
            HIRStatement::Return(value) => value.all_reference(),
            HIRStatement::Continue(_) => Vec::new(),
            HIRStatement::Break(_) => Vec::new(),
        }
    }
}

pub fn resolve_statements(statements: Vec<HIRStatement<SearchPaths>>, resolver: &mut impl KirlNameResolver) -> Vec<HIRStatement<ResolvedItems>> {
    statements.resolve(resolver)
}

pub fn statement_references(statements: &Vec<HIRStatement<SearchPaths>>) -> Vec<&[String]> {
    statements.all_reference()
}
