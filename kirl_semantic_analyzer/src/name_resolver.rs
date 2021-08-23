use std::collections::{BTreeMap, HashMap};
use std::ops::DerefMut;

use uuid::Uuid;

use crate::{HIRExpression, HIRStatement, HIRType, ReferenceAccess, Variable};

pub trait KirlNameResolver {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)>;
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

impl<R: DerefMut> KirlNameResolver for HashMap<String, R>
where
    R::Target: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        full_path.first().and_then(|key| self.get_mut(key)).map(|resolver| resolver.resolve(&full_path[1..])).unwrap_or_default()
    }
}

impl<R: DerefMut> KirlNameResolver for BTreeMap<String, R>
where
    R::Target: KirlNameResolver,
{
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        full_path.first().and_then(|key| self.get_mut(key)).map(|resolver| resolver.resolve(&full_path[1..])).unwrap_or_default()
    }
}

trait Resolvable {
    type ResolveResult;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult;
}

impl<T: Resolvable> Resolvable for Vec<T> {
    type ResolveResult = Vec<T::ResolveResult>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        self.into_iter().map(|v| v.resolve(resolver)).collect()
    }
}

impl<T: Resolvable, U: Resolvable> Resolvable for (T, U) {
    type ResolveResult = (T::ResolveResult, U::ResolveResult);
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        let (t, u) = self;
        (t.resolve(resolver), u.resolve(resolver))
    }
}

impl Resolvable for Variable<Vec<Vec<String>>> {
    type ResolveResult = Variable<Vec<(Uuid, HIRType)>>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            Variable::Named(paths) => Variable::Named(paths.into_iter().flat_map(|path| resolver.resolve(&path)).collect()),
            Variable::Unnamed(id) => Variable::Unnamed(id),
        }
    }
}

impl Resolvable for ReferenceAccess<Vec<Vec<String>>> {
    type ResolveResult = ReferenceAccess<Vec<(Uuid, HIRType)>>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            ReferenceAccess::Variable(variable) => ReferenceAccess::Variable(variable.resolve(resolver)),
            ReferenceAccess::Member(variable, member) => ReferenceAccess::Member(variable.resolve(resolver), member),
        }
    }
}

impl Resolvable for HIRExpression<Vec<Vec<String>>> {
    type ResolveResult = HIRExpression<Vec<(Uuid, HIRType)>>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            HIRExpression::Immediate(value) => HIRExpression::Immediate(value),
            HIRExpression::CallFunction { function, arguments } => HIRExpression::CallFunction {
                function: function.resolve(resolver),
                arguments: arguments.resolve(resolver),
            },
            HIRExpression::AccessVariable(variable) => HIRExpression::AccessVariable(variable.resolve(resolver)),
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
}

impl Resolvable for HIRStatement<Vec<Vec<String>>> {
    type ResolveResult = HIRStatement<Vec<(Uuid, HIRType)>>;
    fn resolve(self, resolver: &mut impl KirlNameResolver) -> Self::ResolveResult {
        match self {
            HIRStatement::Binding { variable_id, variable_type, expression } => HIRStatement::Binding { variable_id, variable_type, expression: expression.resolve(resolver) },
            HIRStatement::Return(value) => HIRStatement::Return(value.map(|value| value.resolve(resolver))),
            HIRStatement::Continue(label) => HIRStatement::Continue(label),
            HIRStatement::Break(label) => HIRStatement::Break(label),
        }
    }
}

pub fn resolve_statements(statements: Vec<HIRStatement<Vec<Vec<String>>>>, resolver: &mut impl KirlNameResolver) -> Vec<HIRStatement<Vec<(Uuid, HIRType)>>> {
    statements.resolve(resolver)
}
