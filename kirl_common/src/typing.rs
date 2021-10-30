use std::borrow::Cow;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone, Ord, PartialOrd, Eq)]
pub enum HIRType {
    Infer,
    Unreachable,
    GenericsTypeArgument(usize),
    Named { path: Vec<String>, generics_arguments: Vec<HIRType> },
    Tuple(Vec<HIRType>),
    Array(Box<HIRType>),
    Function { arguments: Vec<HIRType>, result: Box<HIRType> },
    AnonymousStruct(BTreeMap<String, HIRType>),
    Or(Vec<HIRType>),
}

impl HIRType {
    pub fn apply_generics_type_argument(&self, type_arguments: &[HIRType]) -> Option<HIRType> {
        match self {
            HIRType::Infer => HIRType::Infer,
            HIRType::Unreachable => HIRType::Unreachable,
            HIRType::GenericsTypeArgument(i) => type_arguments.get(*i)?.clone(),
            HIRType::Named { path, generics_arguments } => HIRType::Named {
                path: path.clone(),
                generics_arguments: {
                    let mut types = Vec::with_capacity(generics_arguments.len());
                    for ty in generics_arguments {
                        types.push(ty.apply_generics_type_argument(type_arguments)?);
                    }
                    types
                },
            },
            HIRType::Tuple(items) => HIRType::Tuple({
                let mut types = Vec::with_capacity(items.len());
                for ty in items {
                    types.push(ty.apply_generics_type_argument(type_arguments)?);
                }
                types
            }),
            HIRType::Array(item) => HIRType::Array(Box::new(item.apply_generics_type_argument(type_arguments)?)),
            HIRType::Function { arguments, result } => HIRType::Function {
                arguments: {
                    let mut types = Vec::with_capacity(arguments.len());
                    for ty in arguments {
                        types.push(ty.apply_generics_type_argument(type_arguments)?);
                    }
                    types
                },
                result: Box::new(result.apply_generics_type_argument(type_arguments)?),
            },
            HIRType::AnonymousStruct(members) => HIRType::AnonymousStruct({
                let mut result = BTreeMap::new();
                for (key, ty) in members {
                    result.insert(key.clone(), ty.apply_generics_type_argument(type_arguments)?);
                }
                result
            }),
            HIRType::Or(items) => HIRType::Or({
                let mut types = Vec::with_capacity(items.len());
                for ty in items {
                    types.push(ty.apply_generics_type_argument(type_arguments)?);
                }
                types
            }),
        }
        .into()
    }

    pub fn is_a(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (HIRType::Infer, _) => true,
            (_, HIRType::Infer) => true,
            (HIRType::Unreachable, _) => true,
            (_, HIRType::Unreachable) => false,
            (HIRType::GenericsTypeArgument(i), HIRType::GenericsTypeArgument(j)) => j == i,
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

    pub fn into_normalized(mut self) -> HIRType {
        self.normalize();
        self
    }

    pub fn member_type(&self, member_name: &str) -> Option<Cow<HIRType>> {
        match self {
            HIRType::Infer => Some(Cow::Owned(HIRType::Infer)),
            HIRType::AnonymousStruct(members) => members.get(member_name).map(Cow::Borrowed),
            HIRType::Or(items) => {
                let mut result = Vec::with_capacity(items.len());
                for ty in items.iter().map(|ty| ty.member_type(member_name)) {
                    result.push(ty?.into_owned());
                }
                Some(Cow::Owned(HIRType::Or(result)))
            }
            _ => None,
        }
    }

    pub fn tuple_item_type(&self, n: usize) -> Option<Cow<HIRType>> {
        match self {
            HIRType::Infer => Some(Cow::Owned(HIRType::Infer)),
            HIRType::Tuple(items) => items.get(n).map(Cow::Borrowed),
            HIRType::Or(items) => {
                let mut result = Vec::with_capacity(items.len());
                for ty in items.iter().map(|ty| ty.tuple_item_type(n)) {
                    result.push(ty?.into_owned());
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

    pub fn has_tuple_item(&self, n: usize) -> bool {
        match self {
            HIRType::Infer => true,
            HIRType::Tuple(items) => n < items.len(),
            HIRType::Or(items) => items.iter().all(|ty| ty.has_tuple_item(n)),
            _ => false,
        }
    }

    pub fn intersect_to(&self, rhs: &HIRType) -> HIRType {
        match (self, rhs) {
            (this, rhs) if this == rhs => this.clone(),
            (HIRType::Infer, rhs) => rhs.clone(),
            (this, HIRType::Infer) => this.clone(),
            (HIRType::Tuple(items1), HIRType::Tuple(items2)) if items1.len() <= items2.len() => {
                let mut types = Vec::with_capacity(items1.len());
                for (ty1, ty2) in items1.iter().zip(items2) {
                    types.push(ty1.intersect_to(ty2));
                }
                HIRType::Tuple(types)
            }
            (HIRType::Array(t1), HIRType::Array(t2)) => HIRType::Array(Box::new(t1.intersect_to(t2))),
            (ty1 @ HIRType::AnonymousStruct(members1), HIRType::AnonymousStruct(members2)) => {
                let mut result_members = BTreeMap::new();
                for (key, value) in members1 {
                    if let Some(ty2) = members2.get(key) {
                        result_members.insert(key.clone(), value.intersect_to(ty2));
                    } else {
                        return ty1.clone();
                    }
                }
                HIRType::AnonymousStruct(result_members)
            }
            (HIRType::Or(items1), ty2) => {
                let mut types = Vec::with_capacity(items1.len());
                for ty in items1 {
                    types.push(ty.intersect_to(ty2));
                }
                HIRType::Or(types)
            }
            (ty1 @ HIRType::Tuple(items1), HIRType::Or(types)) => {
                let types = types
                    .iter()
                    .filter_map(|ty| match ty {
                        ty @ HIRType::Tuple(items2) if ty.is_a(ty1) => Some(items1.iter().zip(items2).map(|(ty1, ty2)| ty1.intersect_to(ty2)).collect::<Vec<_>>()),
                        _ => None,
                    })
                    .fold(vec![Vec::new(); items1.len()], |mut acc, types| {
                        acc.iter_mut().zip(types).for_each(|(acc, ty)| acc.push(ty));
                        acc
                    });
                let types = types.into_iter().map(HIRType::Or).collect();
                HIRType::Tuple(types)
            }
            (HIRType::Array(t1), HIRType::Or(types)) => {
                let types = types
                    .iter()
                    .filter_map(|ty| match ty {
                        HIRType::Array(ty) if ty.is_a(t1) => Some(t1.intersect_to(ty)),
                        _ => None,
                    })
                    .collect();
                HIRType::Array(Box::new(HIRType::Or(types).into_normalized()))
            }
            (ty1 @ HIRType::AnonymousStruct(members1), HIRType::Or(types)) => {
                let members = types
                    .iter()
                    .filter_map(|ty| match ty {
                        ty @ HIRType::AnonymousStruct(members2) if ty.is_a(ty1) => Some(members1.iter().map(|(key, ty1)| (key.clone(), ty1.intersect_to(members2.get(key).unwrap()))).collect::<BTreeMap<_, _>>()),
                        _ => None,
                    })
                    .fold(BTreeMap::new(), |mut acc, members| {
                        for (key, ty) in members {
                            acc.entry(key).or_insert_with(Vec::new).push(ty);
                        }
                        acc
                    });
                let members = members.into_iter().map(|(k, t)| (k, HIRType::Or(t).into_normalized())).collect::<BTreeMap<_, _>>();
                HIRType::AnonymousStruct(members)
            }
            (ty1, _) => ty1.clone(),
        }
    }

    pub fn possibility_assignable_to(&self, target: &HIRType) -> bool {
        match (self, target) {
            (ty1, ty2) if ty1 == ty2 => true,
            (HIRType::Infer, _) | (_, HIRType::Infer) => true,
            (HIRType::GenericsTypeArgument(_), _) | (_, HIRType::GenericsTypeArgument(_)) => true,
            (HIRType::Tuple(items1), HIRType::Tuple(items2)) => items1.iter().zip(items2).all(|(ty1, ty2)| ty1.possibility_assignable_to(ty2)),
            (HIRType::Array(ty1), HIRType::Array(ty2)) => ty1.possibility_assignable_to(ty2),
            (HIRType::AnonymousStruct(members1), HIRType::AnonymousStruct(members2)) => !members2.iter().any(|(key, ty2)| members1.get(key).map(|ty1| !ty1.possibility_assignable_to(ty2)).unwrap_or_default()),
            (HIRType::Or(types), ty2) => types.iter().any(|ty1| ty1.possibility_assignable_to(ty2)),
            (ty1, HIRType::Or(types)) => types.iter().any(|ty2| ty1.possibility_assignable_to(ty2)),
            _ => false,
        }
    }

    pub fn infer_temporary(&self) -> HIRType {
        match self {
            HIRType::Infer => HIRType::Tuple(Vec::new()),
            ty @ (HIRType::Unreachable | HIRType::GenericsTypeArgument(_) | HIRType::Named { .. }) => ty.clone(),
            HIRType::Tuple(items) => HIRType::Tuple(items.iter().map(HIRType::infer_temporary).collect()),
            HIRType::Array(item) => HIRType::Array(Box::new(item.infer_temporary())),
            HIRType::Function { arguments, result } => HIRType::Function {
                arguments: arguments.iter().map(HIRType::infer_temporary).collect(),
                result: Box::new(result.infer_temporary()),
            },
            HIRType::AnonymousStruct(members) => HIRType::AnonymousStruct(members.iter().map(|(k, ty)| (k.clone(), ty.infer_temporary())).collect()),
            HIRType::Or(items) => HIRType::Or(items.iter().map(HIRType::infer_temporary).collect()),
        }
    }
}

impl ToString for HIRType {
    fn to_string(&self) -> String {
        match self {
            HIRType::Infer => "$Infer".to_string(),
            HIRType::Unreachable => "!".to_string(),
            HIRType::GenericsTypeArgument(i) => format!("$T{}", i),
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
            HIRType::GenericsTypeArgument(_) => Err(LIRTypeConvertError::UnSupportedType("GenericsTypeArgument")),
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

#[cfg(test)]
mod tests {
    use super::HIRType;
    use std::borrow::Cow;
    use std::collections::BTreeMap;

    #[test]
    fn test_type_is_a() {
        use HIRType::*;
        assert!(Infer.is_a(&Infer));
        assert!(Infer.is_a(&Named { path: vec!["Number".to_string()], generics_arguments: vec![] }));
        assert!(Named { path: vec!["Number".to_string()], generics_arguments: vec![] }.is_a(&Infer));

        assert!(Named { path: vec!["Number".to_string()], generics_arguments: vec![] }.is_a(&Named { path: vec!["Number".to_string()], generics_arguments: vec![] }));
        assert!(!Named { path: vec!["Number".to_string()], generics_arguments: vec![] }.is_a(&Named { path: vec!["String".to_string()], generics_arguments: vec![] }));

        assert!(Tuple(vec![]).is_a(&Tuple(vec![])));
        assert!(Tuple(vec![Tuple(vec![]), Tuple(vec![])]).is_a(&Tuple(vec![Tuple(vec![]), Tuple(vec![])])));
        assert!(Tuple(vec![Tuple(vec![]), Tuple(vec![])]).is_a(&Tuple(vec![Tuple(vec![])])));
        assert!(!Tuple(vec![Tuple(vec![])]).is_a(&Tuple(vec![Tuple(vec![]), Tuple(vec![])])));

        assert!(Array(Box::new(Tuple(vec![Tuple(vec![])]))).is_a(&Array(Box::new(Tuple(vec![])))));
        assert!(!Array(Box::new(Tuple(vec![]))).is_a(&Array(Box::new(Tuple(vec![Tuple(vec![])])))));

        assert!(Function {
            arguments: vec![Tuple(vec![])],
            result: Box::new(Tuple(vec![Tuple(vec![])])),
        }
        .is_a(&Function {
            arguments: vec![Tuple(vec![Tuple(vec![])])],
            result: Box::new(Tuple(vec![])),
        }));
        assert!(!Function {
            arguments: vec![Tuple(vec![]), Tuple(vec![])],
            result: Box::new(Tuple(vec![Tuple(vec![])])),
        }
        .is_a(&Function {
            arguments: vec![Tuple(vec![Tuple(vec![])])],
            result: Box::new(Tuple(vec![])),
        }));

        assert!(AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])])), ("b".to_string(), Tuple(vec![]))])).is_a(&AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![]))]))));
        assert!(!AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![]))])).is_a(&AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])])), ("b".to_string(), Tuple(vec![]))]))));

        assert!(Or(vec![Tuple(vec![Tuple(vec![])]), Tuple(vec![Tuple(vec![]), Tuple(vec![])])]).is_a(&Tuple(vec![Tuple(vec![])])));
        assert!(Tuple(vec![Tuple(vec![]), Tuple(vec![])]).is_a(&Or(vec![Tuple(vec![Tuple(vec![]), Tuple(vec![])]), Tuple(vec![Tuple(vec![])])])));
        assert!(Or(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])]), Tuple(vec![Tuple(vec![Tuple(vec![]), Tuple(vec![])])])]).is_a(&Or(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])])])));
    }

    #[test]
    fn test_type_normalize() {
        use HIRType::*;
        assert_eq!(Or(vec![Tuple(vec![]), Tuple(vec![])]).into_normalized(), Tuple(vec![]));
        assert_eq!(Or(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])])]).into_normalized(), Or(vec![Tuple(vec![Tuple(vec![])]), Tuple(vec![])]).into_normalized());
    }

    #[test]
    fn test_type_member_type() {
        use HIRType::*;
        assert_eq!(AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])])), ("b".to_string(), Tuple(vec![]))])).member_type("a").map(Cow::into_owned), Some(Tuple(vec![Tuple(vec![])])));
        assert_eq!(AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])])), ("b".to_string(), Tuple(vec![]))])).member_type("b").map(Cow::into_owned), Some(Tuple(vec![])));
        assert_eq!(Tuple(vec![]).member_type("a"), None);
        assert_eq!(
            Or(vec![AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])]))])), AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![]))]))]).member_type("a").map(Cow::into_owned).map(HIRType::into_normalized),
            Some(Or(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])])]))
        );
        assert_eq!(Or(vec![AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])]))])), Tuple(vec![])]).member_type("a"), None);
    }

    #[test]
    fn test_type_tuple_item_type() {
        use HIRType::*;
        assert_eq!(Tuple(vec![Tuple(vec![])]).tuple_item_type(0).map(Cow::into_owned), Some(Tuple(vec![])));
        assert_eq!(Tuple(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])])]).tuple_item_type(1).map(Cow::into_owned), Some(Tuple(vec![Tuple(vec![])])));
        assert_eq!(Tuple(vec![Tuple(vec![])]).tuple_item_type(1), None);
        assert_eq!(Or(vec![Tuple(vec![Tuple(vec![Tuple(vec![])])]), Tuple(vec![Tuple(vec![])])]).tuple_item_type(0).map(Cow::into_owned).map(HIRType::into_normalized), Some(Or(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])])])));
        assert_eq!(Or(vec![Tuple(vec![Tuple(vec![])]), Tuple(vec![])]).tuple_item_type(0), None);
    }

    #[test]
    fn test_type_has_member() {
        use HIRType::*;
        assert!(AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])])), ("b".to_string(), Tuple(vec![]))])).has_member("a"));
        assert!(AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])])), ("b".to_string(), Tuple(vec![]))])).has_member("b"));
        assert!(!Tuple(vec![]).has_member("a"));
        assert!(Or(vec![AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])]))])), AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![]))]))]).has_member("a"));
        assert!(!Or(vec![AnonymousStruct(BTreeMap::from([("a".to_string(), Tuple(vec![Tuple(vec![])]))])), Tuple(vec![])]).has_member("a"));
    }

    #[test]
    fn test_type_has_tuple_item() {
        use HIRType::*;
        assert!(Tuple(vec![Tuple(vec![])]).has_tuple_item(0));
        assert!(Tuple(vec![Tuple(vec![]), Tuple(vec![Tuple(vec![])])]).has_tuple_item(1));
        assert!(!Tuple(vec![Tuple(vec![])]).has_tuple_item(1));
        assert!(Or(vec![Tuple(vec![Tuple(vec![Tuple(vec![])])]), Tuple(vec![Tuple(vec![])])]).has_tuple_item(0));
        assert!(!Or(vec![Tuple(vec![Tuple(vec![])]), Tuple(vec![])]).has_tuple_item(0));
    }

    #[test]
    fn test_type_intersect_to() {
        use HIRType::*;
        let tuple0 = Tuple(vec![]);
        let tuple1 = Tuple(vec![tuple0.clone()]);
        let tuple2 = Tuple(vec![tuple0.clone(), tuple0.clone()]);
        assert_eq!(Infer.intersect_to(&Named { path: vec!["Number".to_string()], generics_arguments: vec![] }), Named { path: vec!["Number".to_string()], generics_arguments: vec![] });
        assert_eq!(tuple0.intersect_to(&Named { path: vec!["Number".to_string()], generics_arguments: vec![] }), tuple0);
        assert_eq!(Array(Box::new(Infer)).intersect_to(&Or(vec![Array(Box::new(tuple0.clone())), Array(Box::new(tuple1.clone())), tuple0.clone(), tuple1.clone()])), Array(Box::new(Or(vec![tuple0.clone(), tuple1.clone()]))));
        assert_eq!(
            Tuple(vec![Infer, Infer]).intersect_to(&Or(vec![Tuple(vec![tuple0.clone(), tuple0.clone()]), Tuple(vec![tuple1.clone(), tuple1.clone(), tuple2.clone()]), Tuple(vec![tuple2.clone()])])),
            Tuple(vec![Or(vec![tuple0.clone(), tuple1.clone()]), Or(vec![tuple0.clone(), tuple1.clone()])])
        );
        assert_eq!(
            AnonymousStruct(BTreeMap::from([("a".to_string(), Infer), ("b".to_string(), tuple0.clone())])).intersect_to(&Or(vec![
                AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple1.clone())])),
                AnonymousStruct(BTreeMap::from([("a".to_string(), tuple1.clone()), ("b".to_string(), tuple0.clone())])),
                AnonymousStruct(BTreeMap::from([("b".to_string(), tuple1.clone())])),
                tuple0.clone(),
            ])),
            AnonymousStruct(BTreeMap::from([("a".to_string(), Or(vec![tuple0.clone(), tuple1.clone()])), ("b".to_string(), tuple0.clone())]))
        );
    }

    #[test]
    fn test_type_possibility_assignable_to() {
        use HIRType::*;
        let tuple0 = Tuple(vec![]);
        let tuple1 = Tuple(vec![tuple0.clone()]);
        let tuple2 = Tuple(vec![tuple0.clone(), tuple0.clone()]);
        assert!(Infer.possibility_assignable_to(&Infer));
        assert!(Infer.possibility_assignable_to(&tuple0));
        assert!(tuple0.possibility_assignable_to(&Infer));

        assert!(tuple0.possibility_assignable_to(&tuple1));
        assert!(tuple1.possibility_assignable_to(&tuple2));
        assert!(!tuple1.possibility_assignable_to(&Tuple(vec![Array(Box::new(tuple0.clone()))])));

        assert!(Array(Box::new(tuple1.clone())).possibility_assignable_to(&Array(Box::new(tuple0.clone()))));
        assert!(Array(Box::new(tuple1.clone())).possibility_assignable_to(&Array(Box::new(tuple1.clone()))));
        assert!(Array(Box::new(tuple1.clone())).possibility_assignable_to(&Array(Box::new(tuple2.clone()))));

        assert!(tuple1.possibility_assignable_to(&Or(vec![tuple1.clone(), tuple2.clone()])));
        assert!(Or(vec![tuple1.clone(), tuple2.clone()]).possibility_assignable_to(&tuple1));
        assert!(tuple1.possibility_assignable_to(&Or(vec![tuple0.clone(), tuple2.clone()])));
        assert!(Or(vec![tuple0.clone(), tuple2.clone()]).possibility_assignable_to(&tuple1));

        assert!(AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone())])).possibility_assignable_to(&AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple0.clone())]))));
        assert!(!AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone())])).possibility_assignable_to(&AnonymousStruct(BTreeMap::from([("a".to_string(), Array(Box::new(tuple0.clone()))), ("b".to_string(), tuple0.clone())]))));
        assert!(AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple1.clone())])).possibility_assignable_to(&AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple0.clone())]))));
        assert!(AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple1.clone())])).possibility_assignable_to(&Or(vec![AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple0.clone())])), tuple0.clone()])));
        assert!(tuple2.possibility_assignable_to(&Or(vec![AnonymousStruct(BTreeMap::from([("a".to_string(), tuple0.clone()), ("b".to_string(), tuple0.clone())])), tuple0.clone()])));
    }
}
