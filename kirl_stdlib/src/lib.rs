use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Mutex, RwLock};

use dec::Decimal128;
use once_cell::sync::Lazy;
use uuid::Uuid;

use kirl_semantic_analyzer::name_resolver::KirlNameResolver;
use kirl_semantic_analyzer::HIRType;
use kirl_vm::bytecode::{FunctionWrapper, KirlRustFunction, KirlVMValueCloneable};

enum FunctionOrChildren {
    Function(Arc<Mutex<dyn KirlRustFunction>>, Uuid, HIRType),
    StaticValue(Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>> + Send + Sync>, Uuid, HIRType),
    Children(HashMap<String, Vec<FunctionOrChildren>>),
}

impl Debug for FunctionOrChildren {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionOrChildren::Function(_, id, ty) => write!(f, "{}: {:?}", id, ty),
            FunctionOrChildren::StaticValue(_, id, ty) => write!(f, "{}: {:?}", id, ty),
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

impl<F, Value: KirlVMValueCloneable> From<F> for FunctionOrChildren
where
    F: 'static + Fn() -> Value + Send + Sync,
{
    fn from(closure: F) -> Self {
        FunctionOrChildren::StaticValue(Arc::new(move || Arc::new(RwLock::new(closure()))), Uuid::new_v4(), Value::static_type().into_owned().into())
    }
}

#[derive(Debug)]
pub struct KirlStdLib(HashMap<String, Vec<FunctionOrChildren>>);

impl<'a> KirlNameResolver for &'a FunctionOrChildren {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(uuid::Uuid, HIRType)> {
        match self {
            FunctionOrChildren::Function(_, id, ty) | FunctionOrChildren::StaticValue(_, id, ty) => {
                if full_path.is_empty() {
                    vec![(*id, ty.clone())]
                } else {
                    Vec::new()
                }
            }
            FunctionOrChildren::Children(children) => resolve_ref(children, full_path),
        }
    }
}

impl<'a> KirlNameResolver for &'a KirlStdLib {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        resolve_ref(&self.0, full_path)
    }
}

fn resolve_ref(map: &HashMap<String, Vec<FunctionOrChildren>>, full_path: &[String]) -> Vec<(uuid::Uuid, HIRType)> {
    full_path.first().and_then(|key| map.get(key)).into_iter().flatten().flat_map(|mut resolver| resolver.resolve(&full_path[1..])).collect()
}

struct KirlStdLibFunctions<'a> {
    functions: Vec<(Uuid, Arc<Mutex<dyn KirlRustFunction>>)>,
    maps: Vec<&'a HashMap<String, Vec<FunctionOrChildren>>>,
}

impl<'a> Iterator for KirlStdLibFunctions<'a> {
    type Item = (Uuid, Arc<Mutex<dyn KirlRustFunction>>);

    fn next(&mut self) -> Option<Self::Item> {
        let KirlStdLibFunctions { functions, maps } = self;
        loop {
            if let Some(function) = functions.pop() {
                break Some(function);
            }
            if let Some(map) = maps.pop() {
                for children in map.values().flatten() {
                    match children {
                        FunctionOrChildren::Function(function, id, _) => functions.push((*id, Arc::clone(function))),
                        FunctionOrChildren::Children(children) => maps.push(children),
                        FunctionOrChildren::StaticValue(_, _, _) => {}
                    }
                }
            } else {
                break None;
            }
        }
    }
}

struct KirlStdLibStaticValues<'a> {
    values: Vec<(Uuid, Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>>>)>,
    maps: Vec<&'a HashMap<String, Vec<FunctionOrChildren>>>,
}

impl<'a> Iterator for KirlStdLibStaticValues<'a> {
    type Item = (Uuid, Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>>>);

    fn next(&mut self) -> Option<Self::Item> {
        let KirlStdLibStaticValues { values, maps } = self;
        loop {
            if let Some(function) = values.pop() {
                break Some(function);
            }
            if let Some(map) = maps.pop() {
                for children in map.values().flatten() {
                    match children {
                        FunctionOrChildren::StaticValue(value, id, _) => values.push((*id, Arc::clone(value) as Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>>>)),
                        FunctionOrChildren::Children(children) => maps.push(children),
                        FunctionOrChildren::Function(_, _, _) => {}
                    }
                }
            } else {
                break None;
            }
        }
    }
}

impl KirlStdLib {
    pub fn functions(&self) -> impl IntoIterator<Item = (Uuid, Arc<Mutex<dyn KirlRustFunction>>)> + '_ {
        KirlStdLibFunctions { functions: Vec::new(), maps: vec![&self.0] }
    }

    pub fn static_values(&self) -> impl IntoIterator<Item = (Uuid, Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>>>)> + '_ {
        KirlStdLibStaticValues { values: Vec::new(), maps: vec![&self.0] }
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

static STDLIB: Lazy<KirlStdLib> = Lazy::new(|| {
    KirlStdLib(map! {
        io: map! {
            print: FunctionWrapper::from(|s: String| Ok::<_, NoneError>(print!("{}", s))),
            print: FunctionWrapper::from(|s: Decimal128| Ok::<_, NoneError>(print!("{}", s.to_standard_notation_string()))),
            print: FunctionWrapper::from(|s: bool| Ok::<_, NoneError>(print!("{}", s))),
            println: FunctionWrapper::from(|s: String| Ok::<_, NoneError>(println!("{}", s))),
            println: FunctionWrapper::from(|s: Decimal128| Ok::<_, NoneError>(println!("{}", s.to_standard_notation_string()))),
            println: FunctionWrapper::from(|s: bool| Ok::<_, NoneError>(println!("{}", s))),
        },
        bool: map!{
            _not: FunctionWrapper::from(|a: bool| Ok::<_, NoneError>(!a)),
            _or: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a | b)),
            _and: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a & b)),
            _xor: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a ^ b)),
            _eq: FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a == b)),
            true: ||true,
            false: ||false,
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
    })
});

pub fn get_stdlib() -> &'static KirlStdLib {
    &*STDLIB
}
