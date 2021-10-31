use crate::typing::{HIRType, LIRType};
use dec::Decimal128;
use once_cell::sync::Lazy;
use std::any::{Any, TypeId};
use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

pub trait KirlRustFunction: Send + Sync {
    fn static_type() -> Cow<'static, HIRType>
    where
        Self: Sized;
    fn argument_count(&self) -> usize;
    fn call(&mut self, args: Vec<Arc<RwLock<dyn KirlVMValueCloneable>>>) -> Result<Arc<RwLock<dyn KirlVMValueCloneable>>, Box<dyn Error>>;
}

pub struct FunctionWrapper<Args, Result, F>(F, PhantomData<(Args, Result)>);

macro_rules! count {
    ()=>{ 0 };
    ($t:ident)=>{ 1 };
    ($f:ident,$($t:ident),*)=>{ 1 + count!($($t),*) };
}

macro_rules! tuple {
    ()=>{()};
    ($($t:ident),+)=>{($($t),+,)};
}

macro_rules! impl_fn {
    ($($t:ident),*)=>{
        impl<R, F, $($t),*> From<F> for FunctionWrapper<tuple!($($t),*), R, F>
            where F: FnMut($($t),*) -> R {
            fn from(function: F) -> Self {
                FunctionWrapper(function, PhantomData::default())
            }
        }
        impl<R, E, F, $($t),*> KirlRustFunction for FunctionWrapper<tuple!($($t),*), Result<R, E>, F>
            where F: FnMut($($t),*) -> Result<R, E>,
                  R: InterchangeKirlVMValue,
                  E: std::error::Error + 'static,
                  Self: Send + Sync,
                  $($t: InterchangeKirlVMValue + Clone),* {
            fn static_type() -> Cow<'static, HIRType> {
                Cow::Owned(LIRType::Function { arguments: vec![$($t::static_type().into_owned()),*], result: Box::new(R::static_type().into_owned()) }.into())
            }
            fn argument_count(&self) -> usize { count!($($t),*) }
            fn call(&mut self, args: Vec<Arc<RwLock<dyn KirlVMValueCloneable>>>) -> Result<Arc<RwLock<dyn KirlVMValueCloneable>>, Box<dyn Error>> {
                #[allow(non_snake_case)]
                if let Ok([$($t),*]) = <[_;count!($($t),*)] as std::convert::TryFrom<_>>::try_from(args) {
                    self.0($({
                        let value_ref = $t::try_from_kirl_value($t).unwrap_or_else(|value:Arc<RwLock<dyn KirlVMValueCloneable>>|panic!("expected type {:?} but found {:?}.", std::any::type_name::<$t>(), value.read().unwrap().type_name()));
                        Arc::try_unwrap(value_ref).map(|lock| lock.into_inner().expect("")).unwrap_or_else(|arc| arc.read().expect("").clone())
                    }),*).map(|result|result.into_kirl_value()).map_err(|err|Box::new(err) as Box<dyn Error>)
                } else { unreachable!() }
            }
        }
    }
}

fn downcast<U: Sized + 'static>(value: Arc<RwLock<dyn KirlVMValueCloneable>>) -> Result<Arc<RwLock<U>>, Arc<RwLock<dyn KirlVMValueCloneable>>> {
    let id = <dyn KirlVMValueCloneable>::type_id(&*value.read().unwrap());
    if id == TypeId::of::<U>() {
        unsafe {
            let raw = Arc::into_raw(value);
            Ok(Arc::from_raw(raw as *const RwLock<U>))
        }
    } else {
        Err(value)
    }
}

impl_fn!();
impl_fn!(A1);
impl_fn!(A1, A2);
impl_fn!(A1, A2, A3);
impl_fn!(A1, A2, A3, A4);
impl_fn!(A1, A2, A3, A4, A5);

macro_rules! get_type {
    (!)=>{ kirl_common::typing::LIRType::Unreachable };
    (($($id:ident)::+ $(::<$($ty:tt),*>)?))=>{ $crate::typing::LIRType::Named{path:vec![$(std::string::String::from(stringify!($id))),+], generics_arguments: vec![$($(get_type!($ty)),*)?]} };
    ((($($ty:tt),*)->($r:tt)))=>{ $crate::typing::LIRType::Function{arguments: vec![$(get_type!($ty)),*], result: Box::new(get_type!($r))} };
    (())=>{ $crate::typing::LIRType::Tuple(Vec::new()) };
    (($($ty:tt),*,))=>{ $crate::typing::LIRType::Tuple(vec![$(get_type!($ty)),*]) };
    ([$ty:tt])=>{ $crate::typing::LIRType::Array(Box::new(get_type!($ty))) };
    ((#{$($name:ident : $ty:tt),*$(,)?}))=>{
        {
            let map = std::collections::BTreeMap::<std::string::String, $crate::typing::LIRType>::new();
            $(
                map.insert(std::string::String::from(stringify!($name)), get_type!($ty)).expect("duplicated name");
            )*
            $crate::typing::LIRType::AnonymousStruct(map)
        }
    };
    (($($ty:tt)|*|))=>{ $crate::typing::LIRType::Or(vec![get_type!($ty)]) };
}

pub trait InterchangeKirlVMValue: Sized {
    fn static_type() -> Cow<'static, LIRType>;
    fn get_type(&self) -> Cow<LIRType>;
    fn into_kirl_value(self) -> Arc<RwLock<dyn KirlVMValueCloneable>>;
    fn try_from_kirl_value(value: Arc<RwLock<dyn KirlVMValueCloneable>>) -> Result<Arc<RwLock<Self>>, Arc<RwLock<dyn KirlVMValueCloneable>>>;
}

impl<T: Sized + KirlVMValueCloneable> InterchangeKirlVMValue for T {
    fn static_type() -> Cow<'static, LIRType> {
        Self::static_type()
    }

    fn get_type(&self) -> Cow<LIRType> {
        KirlVMValue::get_type(self)
    }

    fn into_kirl_value(self) -> Arc<RwLock<dyn KirlVMValueCloneable>> {
        Arc::new(RwLock::new(self))
    }

    fn try_from_kirl_value(value: Arc<RwLock<dyn KirlVMValueCloneable>>) -> Result<Arc<RwLock<Self>>, Arc<RwLock<dyn KirlVMValueCloneable>>> {
        downcast(value)
    }
}

pub trait KirlVMValue: Any + Debug + Send + Sync + 'static {
    fn static_type() -> Cow<'static, LIRType>
    where
        Self: Sized;
    fn get_type(&self) -> Cow<LIRType>;
    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl KirlVMValue for String {
    fn static_type() -> Cow<'static, LIRType> {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!((String)).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        <Self as KirlVMValue>::static_type()
    }
}

impl KirlVMValue for Decimal128 {
    fn static_type() -> Cow<'static, LIRType> {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!((Number)).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        <Self as KirlVMValue>::static_type()
    }
}

impl KirlVMValue for () {
    fn static_type() -> Cow<'static, LIRType> {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!(()).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        <Self as KirlVMValue>::static_type()
    }
}

impl KirlVMValue for LIRType {
    fn static_type() -> Cow<'static, LIRType> {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!((Type)).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        <Self as KirlVMValue>::static_type()
    }
}

impl KirlVMValue for bool {
    fn static_type() -> Cow<'static, LIRType> {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!((Bool)).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        <Self as KirlVMValue>::static_type()
    }
}

impl KirlVMValue for Vec<Arc<RwLock<dyn KirlVMValueCloneable>>> {
    fn static_type() -> Cow<'static, LIRType>
    where
        Self: Sized,
    {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!([()]).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        Cow::Owned(LIRType::Array(Box::new(LIRType::Or(self.iter().map(|value| value.read().expect("").get_type().into_owned()).collect()).into_normalized())))
    }
}

impl KirlVMValue for Box<[Arc<RwLock<dyn KirlVMValueCloneable>>]> {
    fn static_type() -> Cow<'static, LIRType>
    where
        Self: Sized,
    {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!(()).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        Cow::Owned(LIRType::Tuple(self.iter().map(|value| value.read().unwrap().get_type().into_owned()).collect()))
    }
}

impl KirlVMValue for HashMap<String, Arc<RwLock<dyn KirlVMValueCloneable>>> {
    fn static_type() -> Cow<'static, LIRType>
    where
        Self: Sized,
    {
        static TYPE: Lazy<LIRType> = Lazy::new(|| get_type!((#{})).into_normalized());
        Cow::Borrowed(&TYPE)
    }

    fn get_type(&self) -> Cow<LIRType> {
        Cow::Owned(LIRType::AnonymousStruct(self.iter().map(|(key, value)| (key.clone(), value.read().expect("").get_type().into_owned())).collect()).into_normalized())
    }
}

pub trait KirlVMValueCloneable: KirlVMValue {
    fn kirl_clone(&self) -> Box<dyn KirlVMValueCloneable>;
}

impl<T: KirlVMValue + Clone> KirlVMValueCloneable for T {
    fn kirl_clone(&self) -> Box<dyn KirlVMValueCloneable> {
        Box::new(self.clone())
    }
}
