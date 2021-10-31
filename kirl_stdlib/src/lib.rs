use kirl_common::dec::Decimal128;
use kirl_common::interface::{FunctionWrapper, KirlRustFunction, KirlVMValueCloneable};
use kirl_common::typing::HIRType;
use kirl_common_macro::kirl_function;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Mutex, RwLock};
use uuid::Uuid;

use kirl_semantic_analyzer::name_resolver::KirlNameResolver;

pub enum FunctionOrChildren {
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

impl FunctionOrChildren {
    fn from_function<F: 'static + KirlRustFunction>(f: F) -> Self {
        FunctionOrChildren::Function(Arc::new(Mutex::new(f)), Uuid::new_v4(), F::static_type().into_owned())
    }

    fn static_value<F: 'static + Fn() -> Value + Send + Sync, Value: KirlVMValueCloneable>(f: F) -> Self {
        FunctionOrChildren::StaticValue(Arc::new(move || Arc::new(RwLock::new(f()))), Uuid::new_v4(), Value::static_type().into_owned().into())
    }
}

impl From<HashMap<String, Vec<FunctionOrChildren>>> for FunctionOrChildren {
    fn from(value: HashMap<String, Vec<FunctionOrChildren>>) -> Self {
        FunctionOrChildren::Children(value)
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
            print: FunctionOrChildren::from_function({
                #[kirl_function((String)->())]
                fn string_print(s: Arc<RwLock<String>>) {
                    print!("{}", s.read().unwrap());
                }
                string_print::new()
            }),
            print: FunctionOrChildren::from_function(FunctionWrapper::from(|s: Decimal128| Ok::<_, NoneError>(print!("{}", s.to_standard_notation_string())))),
            print: FunctionOrChildren::from_function(FunctionWrapper::from(|s: bool| Ok::<_, NoneError>(print!("{}", s)))),
            println: FunctionOrChildren::from_function({
                #[kirl_function((String)->())]
                fn string_println(s: Arc<RwLock<String>>){
                    println!("{}", s.read().unwrap());
                }
                string_println::new()
            }),
            println: FunctionOrChildren::from_function(FunctionWrapper::from(|s: Decimal128| Ok::<_, NoneError>(println!("{}", s.to_standard_notation_string())))),
            println: FunctionOrChildren::from_function(FunctionWrapper::from(|s: bool| Ok::<_, NoneError>(println!("{}", s)))),
        },
        bool: map!{
            _not: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool| Ok::<_, NoneError>(!a))),
            _or: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a | b))),
            _and: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a & b))),
            _xor: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a ^ b))),
            _eq: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a == b))),
            true: FunctionOrChildren::static_value(||true),
            false: FunctionOrChildren::static_value(||false),
        },
        num: map! {
            _add: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a + b))),
            _sub: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a - b))),
            _mul: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a * b))),
            _div: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a / b))),
            _rem: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a % b))),
            _eq: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a == b))),
            _gt: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128, b: Decimal128| Ok::<_, NoneError>(a > b))),
            _neg: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128| Ok::<_, NoneError>(-a))),
        },
    })
});

pub fn get_stdlib() -> &'static KirlStdLib {
    &*STDLIB
}
