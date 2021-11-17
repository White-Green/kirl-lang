use kirl_common::dec::Decimal128;
use kirl_common::interface::{FunctionWrapper, InterchangeKirlVMValue, KirlRustFunction, KirlVMValue, KirlVMValueLock};
use kirl_common::typing::{HIRType, LIRType};
use kirl_common::{dec, get_type};
use kirl_common_macro::kirl_function;
use once_cell::sync::Lazy;
use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::io;
use std::io::Read;
use std::sync::{Arc, Mutex, RwLock};
use uuid::Uuid;

use kirl_semantic_analyzer::name_resolver::KirlNameResolver;

pub enum FunctionOrChildren {
    Function(Arc<Mutex<dyn KirlRustFunction>>, Uuid, HIRType),
    StaticValue(Arc<dyn Fn() -> Arc<dyn KirlVMValueLock> + Send + Sync>, Uuid, HIRType),
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

    fn static_value<F: 'static + Fn() -> Value + Send + Sync, Value: KirlVMValue>(f: F) -> Self {
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
    values: Vec<(Uuid, Arc<dyn Fn() -> Arc<dyn KirlVMValueLock>>)>,
    maps: Vec<&'a HashMap<String, Vec<FunctionOrChildren>>>,
}

impl<'a> Iterator for KirlStdLibStaticValues<'a> {
    type Item = (Uuid, Arc<dyn Fn() -> Arc<dyn KirlVMValueLock>>);

    fn next(&mut self) -> Option<Self::Item> {
        let KirlStdLibStaticValues { values, maps } = self;
        loop {
            if let Some(function) = values.pop() {
                break Some(function);
            }
            if let Some(map) = maps.pop() {
                for children in map.values().flatten() {
                    match children {
                        FunctionOrChildren::StaticValue(value, id, _) => values.push((*id, Arc::clone(value) as Arc<dyn Fn() -> Arc<dyn KirlVMValueLock>>)),
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

    pub fn static_values(&self) -> impl IntoIterator<Item = (Uuid, Arc<dyn Fn() -> Arc<dyn KirlVMValueLock>>)> + '_ {
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
            stdin: map! {
                read_line: FunctionOrChildren::from_function({
                    #[kirl_function(()->String)]
                    fn stdin_read_line() -> Result<String, io::Error> {
                        let mut result = String::new();
                        io::stdin().read_line(&mut result)?;
                        Ok(result)
                    }
                    stdin_read_line::new()
                }),
                read_all: FunctionOrChildren::from_function({
                    #[kirl_function(()->String)]
                    fn stdin_read_all() -> Result<String, io::Error> {
                        let mut result = String::new();
                        io::stdin().read_to_string(&mut result)?;
                        Ok(result)
                    }
                    stdin_read_all::new()
                }) ,
            },
        },
        bool: map!{
            _not: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool| Ok::<_, NoneError>(!a))),
            _or: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a | b))),
            _and: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a & b))),
            _xor: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a ^ b))),
            _eq: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool, b: bool| Ok::<_, NoneError>(a == b))),
            true: FunctionOrChildren::static_value(||true),
            false: FunctionOrChildren::static_value(||false),
            to_string: FunctionOrChildren::from_function(FunctionWrapper::from(|a: bool| Ok::<_, NoneError>(a.to_string()))),
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
            to_string: FunctionOrChildren::from_function(FunctionWrapper::from(|a: Decimal128| Ok::<_, NoneError>(a.to_standard_notation_string()))),
        },
        string: map! {
            _eq: FunctionOrChildren::from_function({
                #[kirl_function((String, String)->Bool)]
                fn str_eq(a: Arc<RwLock<String>>, b: Arc<RwLock<String>>) -> bool {
                    str::eq(&**a.read().unwrap(), &**b.read().unwrap())
                }
                str_eq::new()
            }),
            _gt: FunctionOrChildren::from_function({
                #[kirl_function((String, String)->Bool)]
                fn str_gt(a: Arc<RwLock<String>>, b: Arc<RwLock<String>>) -> bool {
                    str::gt(&**a.read().unwrap(), &**b.read().unwrap())
                }
                str_gt::new()
            }),
            regex: map! {
                is_match: FunctionOrChildren::from_function({
                    #[kirl_function((String, String)->Bool)]
                    fn is_match(input: Arc<RwLock<String>>, pattern: Arc<RwLock<String>>)->Result<bool, regex::Error>{
                        let regex = Regex::new(&pattern.read().unwrap())?;
                        Ok(regex.is_match(&input.read().unwrap()))
                    }
                    is_match::new()
                }),
                replace: FunctionOrChildren::from_function({
                    #[kirl_function((String, String, String)->String)]
                    fn replace(input: Arc<RwLock<String>>, pattern: Arc<RwLock<String>>, replace: Arc<RwLock<String>>)->Result<String, regex::Error>{
                        let regex = Regex::new(&pattern.read().unwrap())?;
                        Ok(regex.replace_all(&input.read().unwrap(), &**replace.read().unwrap()).into_owned())
                    }
                    replace::new()
                }),
            },
        },
        collections: map! {
            list: map! {
                new: {
                    #[kirl_function(for<T> ()->[T] )]
                    fn create_list() -> Vec<Arc<dyn KirlVMValueLock>> {
                        Vec::new()
                    }
                    FunctionOrChildren::from_function(create_list::new())
                },
                fill: {
                    #[kirl_function(for<T> (T, Number)->[T] )]
                    fn fill_list(item: Arc<dyn KirlVMValueLock>, count: Decimal128) -> Vec<Arc<dyn KirlVMValueLock>> {
                        vec![item; usize::try_from(dec::Decimal::<15>::from(count)).unwrap()]
                    }
                    FunctionOrChildren::from_function(fill_list::new())
                },
                len: {
                    #[kirl_function(for<T> ([T])->Number )]
                    fn list_length(list: Arc<RwLock<Vec<Arc<dyn KirlVMValueLock>>>>) -> Decimal128 {
                        (list.read().unwrap().len() as u64).into()
                    }
                    FunctionOrChildren::from_function(list_length::new())
                },
                push: {
                    #[kirl_function(for<T> ([T], T)->() )]
                    fn list_push(list: Arc<RwLock<Vec<Arc<dyn KirlVMValueLock>>>>, item: Arc<dyn KirlVMValueLock>) {
                        list.write().unwrap().push(item);
                    }
                    FunctionOrChildren::from_function(list_push::new())
                },
                insert: {
                    #[kirl_function(for<T> ([T], Number, T)->() )]
                    fn list_insert(list: Arc<RwLock<Vec<Arc<dyn KirlVMValueLock>>>>, index: Decimal128, item: Arc<dyn KirlVMValueLock>) {
                        list.write().unwrap().insert(usize::try_from(dec::Decimal::<15>::from(index)).unwrap(), item);
                    }
                    FunctionOrChildren::from_function(list_insert::new())
                },
                remove: {
                    #[kirl_function(for<T> ([T], Number)->() )]
                    fn list_remove(list: Arc<RwLock<Vec<Arc<dyn KirlVMValueLock>>>>, index: Decimal128) {
                        list.write().unwrap().remove(usize::try_from(dec::Decimal::<15>::from(index)).unwrap());
                    }
                    FunctionOrChildren::from_function(list_remove::new())
                },
                _get_item: {
                    #[kirl_function(for<T> ([T], Number)->T )]
                    fn list_get_item(list: Arc<RwLock<Vec<Arc<dyn KirlVMValueLock>>>>, index: Decimal128) -> Arc<dyn KirlVMValueLock> {
                        Arc::clone(&list.read().unwrap()[usize::try_from(dec::Decimal::<15>::from(index)).unwrap()])
                    }
                    FunctionOrChildren::from_function(list_get_item::new())
                },
                _set_item: {
                    #[kirl_function(for<T> ([T], Number, T)->())]
                    fn list_set_item(list: Arc<RwLock<Vec<Arc<dyn KirlVMValueLock>>>>, index: Decimal128, item: Arc<dyn KirlVMValueLock>) {
                        list.write().unwrap()[usize::try_from(dec::Decimal::<15>::from(index)).unwrap()] = item;
                    }
                    FunctionOrChildren::from_function(list_set_item::new())
                },
                _iterator: {
                    #[kirl_function(for<T> ([T])->std::iter::Iterator::<T>)]
                    fn list_iterator(list: Vec<Arc<dyn KirlVMValueLock>>) -> IteratorWrapper {
                        IteratorWrapper::new_kirl_value(list.into_iter(), get_type!((std::iter::Iterator::<(Number)>)))
                    }
                    FunctionOrChildren::from_function(list_iterator::new())
                }
            },
        },
        iter: map! {
            range: {
                #[kirl_function((Number)->std::iter::Iterator::<Number>)]
                fn range(stop: Decimal128) -> IteratorWrapper {
                    let step = Decimal128::from(1);
                    let iter = std::iter::successors(Some(Decimal128::from(0)), move |&i| Some(i + step)).take_while(move |i| i < &stop);
                    IteratorWrapper::new(iter, get_type!((std::iter::Iterator::<(Number)>)))
                }
                FunctionOrChildren::from_function(range::new())
            },
            range: {
                #[kirl_function((Number, Number)->std::iter::Iterator::<Number>)]
                fn range(start: Decimal128, stop: Decimal128) -> IteratorWrapper {
                    let step = Decimal128::from(1);
                    let iter = std::iter::successors(Some(start), move |&i| Some(i + step)).take_while(move |i| i < &stop);
                    IteratorWrapper::new(iter, get_type!((std::iter::Iterator::<(Number)>)))
                }
                FunctionOrChildren::from_function(range::new())
            },
            range: {
                #[kirl_function((Number, Number, Number)->std::iter::Iterator::<Number>)]
                fn range(start: Decimal128, stop: Decimal128, step: Decimal128) -> IteratorWrapper {
                    let iter = std::iter::successors(Some(start), move |&i| Some(i + step)).take_while(move |i| i < &stop);
                    IteratorWrapper::new(iter, get_type!((std::iter::Iterator::<(Number)>)))
                }
                FunctionOrChildren::from_function(range::new())
            },
            _iterator: {
                #[kirl_function(for<T> (std::iter::Iterator::<T>)->std::iter::Iterator::<T> )]
                fn iterator_into_iterator(iter: Arc<dyn KirlVMValueLock>) -> Arc<dyn KirlVMValueLock> {
                    iter
                }
                FunctionOrChildren::from_function(iterator_into_iterator::new())
            },
            _next: {
                #[kirl_function(for<T> (std::iter::Iterator::<T>)->(() | #{ value: T }))]
                fn iterator_into_iterator(iter: Arc<RwLock<IteratorWrapper>>) -> Arc<dyn KirlVMValueLock> {
                    match iter.write().unwrap().iter.next() {
                        Some(value) => {
                            let map = {
                                let mut map = HashMap::new();
                                map.insert("value".to_string(), value);
                                map
                            };
                            map.into_kirl_value()
                        },
                        None => ().into_kirl_value()
                    }
                }
                FunctionOrChildren::from_function(iterator_into_iterator::new())
            },
        },
    })
});

pub fn get_stdlib() -> &'static KirlStdLib {
    &*STDLIB
}

struct IteratorWrapper {
    iter: Box<dyn Iterator<Item = Arc<dyn KirlVMValueLock>> + Send + Sync>,
    iter_type: LIRType,
}

impl Debug for IteratorWrapper {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IteratorWrapper").field("iter", &"emitted").field("iter_type", &self.iter_type).finish()
    }
}

impl IteratorWrapper {
    fn new<I: Iterator + Send + Sync + 'static>(iter: I, item_type: LIRType) -> Self
    where
        I::Item: InterchangeKirlVMValue,
    {
        let iter = Box::new(iter.map(InterchangeKirlVMValue::into_kirl_value));
        let iter_type = LIRType::Named {
            path: vec!["std".to_string(), "iter".to_string(), "Iterator".to_string()],
            generics_arguments: vec![item_type],
        };
        IteratorWrapper { iter, iter_type }
    }

    fn new_kirl_value<I: Iterator<Item = Arc<dyn KirlVMValueLock>> + Send + Sync + 'static>(iter: I, item_type: LIRType) -> Self {
        let iter = Box::new(iter);
        let iter_type = LIRType::Named {
            path: vec!["std".to_string(), "iter".to_string(), "Iterator".to_string()],
            generics_arguments: vec![item_type],
        };
        IteratorWrapper { iter, iter_type }
    }
}

impl KirlVMValue for IteratorWrapper {
    fn static_type() -> Cow<'static, LIRType>
    where
        Self: Sized,
    {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!((std::iter::Iterator::<()>)).into_normalized());
        Cow::Borrowed(&*TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        Cow::Borrowed(&self.iter_type)
    }
}
