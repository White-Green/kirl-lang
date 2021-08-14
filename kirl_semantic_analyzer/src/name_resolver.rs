use uuid::Uuid;

use crate::{HIRExpression, HIRStatement, HIRType, ReferenceAccess, Variable};

pub trait KirlNameResolver {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)>;
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
