use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Mutex};

use dec::Decimal128;
use once_cell::sync::Lazy;
use uuid::Uuid;

use kirl_semantic_analyzer::name_resolver::KirlNameResolver;
use kirl_semantic_analyzer::HIRType;
use kirl_vm::bytecode::{FunctionWrapper, KirlRustFunction};

enum FunctionOrChildren {
    Function(Arc<Mutex<dyn KirlRustFunction>>, Uuid, HIRType),
    Children(HashMap<String, Vec<FunctionOrChildren>>),
}

impl Debug for FunctionOrChildren {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionOrChildren::Function(_, id, ty) => write!(f, "{}: {:?}", id, ty),
            FunctionOrChildren::Children(children) => write!(f, "{:?}", children),
        }
    }
}

impl From<HashMap<String, Vec<FunctionOrChildren>>> for FunctionOrChildren {
    fn from(value: HashMap<String, Vec<FunctionOrChildren>>) -> Self {
        FunctionOrChildren::Children(value)
    }
}

impl From<(Arc<Mutex<dyn KirlRustFunction>>, Uuid, HIRType)> for FunctionOrChildren {
    fn from((v1, v2, v3): (Arc<Mutex<dyn KirlRustFunction>>, Uuid, HIRType)) -> Self {
        FunctionOrChildren::Function(v1, v2, v3)
    }
}

impl<Args, Result, F> From<FunctionWrapper<Args, Result, F>> for FunctionOrChildren
where
    FunctionWrapper<Args, Result, F>: KirlRustFunction + 'static,
{
    fn from(function: FunctionWrapper<Args, Result, F>) -> Self {
        FunctionOrChildren::Function(Arc::new(Mutex::new(function)), Uuid::new_v4(), FunctionWrapper::<Args, Result, F>::static_type().into_owned().into())
    }
}

#[derive(Debug)]
pub struct KirlStdLib(HashMap<String, Vec<FunctionOrChildren>>);

impl KirlNameResolver for FunctionOrChildren {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(uuid::Uuid, HIRType)> {
        match self {
            FunctionOrChildren::Function(_, id, ty) => {
                if full_path.is_empty() {
                    vec![(*id, ty.clone())]
                } else {
                    Vec::new()
                }
            }
            FunctionOrChildren::Children(children) => children.resolve(full_path),
        }
    }
}

impl KirlNameResolver for KirlStdLib {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        self.0.resolve(full_path)
    }
}

impl<'a> IntoIterator for &'a KirlStdLib {
    type Item = (Uuid, Arc<Mutex<dyn KirlRustFunction>>);
    type IntoIter = <Vec<(Uuid, Arc<Mutex<dyn KirlRustFunction>>)> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        fn add(item: &FunctionOrChildren, result: &mut Vec<(Uuid, Arc<Mutex<dyn KirlRustFunction>>)>) {
            match item {
                FunctionOrChildren::Function(function, id, _) => result.push((*id, Arc::clone(function))),
                FunctionOrChildren::Children(children) => {
                    for item in children.values().flatten() {
                        add(item, result);
                    }
                }
            }
        }
        let mut result = Vec::new();
        for item in self.0.values().flatten() {
            add(item, &mut result);
        }
        result.into_iter()
    }
}

macro_rules! count {
    ()=>{ 0 };
    ($t:ident)=>{ 1 };
    ($f:ident,$($t:ident),*)=>{ 1 + count!($($t),*) };
}

macro_rules! map {
    ($($name:ident : $value:expr),* $(,)?)=>{
        {
            let mut result = std::collections::HashMap::<std::string::String, Vec<FunctionOrChildren>>::with_capacity(count!($($name),*));
            $(
                result.entry(std::string::String::from(stringify!($name)))
                    .or_default()
                    .push(($value).into());
            )*
            result.shrink_to_fit();
            result
        }
    }
}

#[derive(Debug)]
struct NoneError;

impl Display for NoneError {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        unreachable!("Errorを返すことが無い場合にNoneError型を使いましょう")
    }
}

impl Error for NoneError {}

static STDLIB: Lazy<Arc<Mutex<KirlStdLib>>> = Lazy::new(|| {
    Arc::new(Mutex::new(KirlStdLib(map! {
        io: map! {
            print: FunctionWrapper::from(|s: String| Ok::<_, NoneError>(print!("{}", s))),
            print: FunctionWrapper::from(|s: Decimal128| Ok::<_, NoneError>(print!("{}", s))),
            print: FunctionWrapper::from(|s: bool| Ok::<_, NoneError>(print!("{}", s))),
            println: FunctionWrapper::from(|s: String| Ok::<_, NoneError>(println!("{}", s))),
            println: FunctionWrapper::from(|s: Decimal128| Ok::<_, NoneError>(println!("{}", s))),
            println: FunctionWrapper::from(|s: bool| Ok::<_, NoneError>(println!("{}", s))),
        },
        bool: map!{
            _not: FunctionWrapper::from(|a: bool| Ok::<_, NoneError>(!a)),
            _or: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a | b)),
            _and: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a & b)),
            _xor: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a ^ b)),
            _eq: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a == b)),
        },
        num: map! {
            _add: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a + b)),
            _sub: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a - b)),
            _mul: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a * b)),
            _div: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a / b)),
            _rem: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a % b)),
            _eq: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a == b)),
            _gt: FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a > b)),
            _neg: FunctionWrapper::from(|a: Decimal128| Ok::<_, NoneError>(-a)),
        },
    })))
});

pub fn get_stdlib() -> &'static Arc<Mutex<KirlStdLib>> {
    &*STDLIB
}
