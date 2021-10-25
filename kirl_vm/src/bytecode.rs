use std::any::{Any, TypeId};
use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::sync::{Arc, Mutex, RwLock};

use dec::{Context, Decimal128};
use once_cell::sync::Lazy;
use uuid::Uuid;

use crate::lir::{LIRInstruction, LIRStatement, LIRType};

#[derive(Debug, Clone, Copy)]
pub struct KirlByteCode(KirlByteCodeOpcode, [u8; 3]);

#[derive(Debug, Clone, Copy)]
pub enum KirlByteCodeOpcode {
    LoadStaticValue,
    Load,
    Store,
    JumpIfTrue,
    JumpIfHasType,
    Jump,
    CallKirlFunction,
    CallRustFunction,
    Return,
    Nop,
    AccessTupleItem,
    AccessMember,
    AssignTupleItem,
    AssignMember,
    ConstructStruct,
    ConstructTuple,
    ConstructArray,
    PushAdditionalOperand,
}

impl KirlByteCode {
    pub fn new(opcode: KirlByteCodeOpcode, operand: u32) -> Self {
        let [x1, x2, x3, x4] = operand.to_le_bytes();
        assert!(x4 == 0xff || x4 == 0x00);
        KirlByteCode(opcode, [x1, x2, x3])
    }

    pub fn without_operand(opcode: KirlByteCodeOpcode) -> Self {
        Self::new(opcode, 0)
    }

    pub fn new_signed(opcode: KirlByteCodeOpcode, operand: i32) -> Self {
        let operand = operand as u32;
        assert_eq!(((operand >> 24) ^ (operand >> 16)) & 0b1000_0000, 0);
        Self::new(opcode, operand)
    }

    pub fn opcode(self) -> KirlByteCodeOpcode {
        self.0
    }

    pub fn operand(self) -> u32 {
        let [x1, x2, x3] = self.1;
        u32::from_le_bytes([x1, x2, x3, 0])
    }

    pub fn operand_signed(self) -> i32 {
        let [x1, x2, x3] = self.1;
        let x4 = if x3 & 0b1000_0000 == 0 { 0 } else { 0xff };
        i32::from_le_bytes([x1, x2, x3, x4])
    }
}

pub trait KirlRustFunction: Send + Sync {
    fn static_type() -> Cow<'static, LIRType>
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
            fn static_type() -> Cow<'static, LIRType> {
                Cow::Owned(LIRType::Function { arguments: vec![$($t::static_type().into_owned()),*], result: Box::new(R::static_type().into_owned()) })
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
    (!)=>{ $crate::lir::LIRType::Unreachable };
    (($($id:ident)::+))=>{ $crate::lir::LIRType::Named{path:vec![$(std::string::String::from(stringify!($id))),+], generics_arguments:Vec::new()} };
    ((($($ty:tt),*)->($r:tt)))=>{ $crate::lir::LIRType::Function{arguments: vec![$(get_type!($ty)),*], result: Box::new(get_type!($r))} };
    (())=>{ $crate::lir::LIRType::Tuple(Vec::new()) };
    (($($ty:tt),*,))=>{ $crate::lir::LIRType::Tuple(vec![$(get_type!($ty)),*]) };
    ([$ty:tt])=>{ $crate::lir::LIRType::Array(Box::new(get_type!($ty))) };
    ((#{$($name:ident : $ty:tt),*$(,)?}))=>{
        {
            let map = std::collections::BTreeMap::<std::string::String, $crate::lir::LIRType>::new();
            $(
                map.insert(std::string::String::from(stringify!($name)), get_type!($ty)).expect("duplicated name");
            )*
            $crate::lir::LIRType::AnonymousStruct(map)
        }
    };
    (($($ty:tt)|*|))=>{ $crate::lir::LIRType::Or(vec![get_type!($ty)]) };
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

pub(crate) type StaticValueGenerator = Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>>>;

pub struct KirlVMExecutable {
    pub(crate) bytecodes: Vec<KirlByteCode>,
    pub(crate) entry_point: usize,
    pub(crate) static_value_generators: Vec<StaticValueGenerator>,
    pub(crate) rust_functions: Vec<Arc<Mutex<dyn KirlRustFunction>>>,
    pub(crate) function_pointers: Vec<usize>,
    pub(crate) member_names: Vec<String>,
    pub(crate) types: Vec<LIRType>,
}

impl KirlVMExecutable {
    pub fn new(functions: impl IntoIterator<Item = (Uuid, Vec<LIRStatement>)>, static_value_generators: impl IntoIterator<Item = (Uuid, Arc<dyn Fn() -> Arc<RwLock<dyn KirlVMValueCloneable>>>)>, rust_functions: impl IntoIterator<Item = (Uuid, Arc<Mutex<dyn KirlRustFunction>>)>, main_function: Uuid) -> Self {
        let (mut static_value_generators, static_value_index): (Vec<_>, HashMap<_, _>) = static_value_generators.into_iter().enumerate().map(|(i, (id, generator))| (generator, (id, u32::try_from(i).unwrap()))).unzip();
        let (rust_functions, rust_function_index): (Vec<_>, HashMap<_, _>) = rust_functions.into_iter().enumerate().map(|(i, (id, function))| (function, (id, u32::try_from(i).unwrap()))).unzip();
        let mut bytecodes = Vec::new();
        let mut function_pointers = HashMap::new();
        let mut member_name_map = HashMap::new();
        let mut type_map = HashMap::new();
        let mut function_references = HashMap::new();
        for (function_id, function_body) in functions {
            function_pointers.insert(function_id, bytecodes.len());
            let (bytecode, function_reference) = lir_to_bytecode(function_body, &mut member_name_map, &mut type_map, &mut static_value_generators, &static_value_index, &rust_function_index);
            function_references.extend(function_reference.into_iter().map(|(index, id)| (index + bytecodes.len(), id)));
            bytecodes.extend(bytecode);
        }
        let (function_pointers, function_pointer_reference): (Vec<_>, HashMap<_, _>) = function_pointers.into_iter().enumerate().map(|(i, (id, pointer))| (pointer, (id, u32::try_from(i).unwrap()))).unzip();
        for (index, function) in function_references {
            let function_pointer = function_pointer_reference[&function];
            let opcode = bytecodes[index].0;
            bytecodes[index] = KirlByteCode::new(opcode, function_pointer);
        }
        let mut member_names = member_name_map.into_iter().collect::<Vec<_>>();
        member_names.sort_unstable_by_key(|(_, index)| *index);
        let member_names = member_names.into_iter().map(|(name, _)| name).collect();
        let mut types = type_map.into_iter().collect::<Vec<_>>();
        types.sort_unstable_by_key(|(_, index)| *index);
        let types = types.into_iter().map(|(ty, _)| ty).collect();
        KirlVMExecutable {
            bytecodes,
            entry_point: function_pointers[function_pointer_reference[&main_function] as usize],
            static_value_generators,
            rust_functions,
            function_pointers,
            member_names,
            types,
        }
    }
}

fn lir_to_bytecode(lir: impl IntoIterator<Item = LIRStatement>, member_name_map: &mut HashMap<String, u32>, type_map: &mut HashMap<LIRType, u32>, static_value_generators: &mut Vec<StaticValueGenerator>, static_value_index: &HashMap<Uuid, u32>, rust_function_index: &HashMap<Uuid, u32>) -> (impl IntoIterator<Item = KirlByteCode>, impl IntoIterator<Item = (usize, Uuid)>) {
    let mut result = Vec::new();
    let mut label_position_map = HashMap::new();
    let mut position_label_map = HashMap::new();
    let mut function_pointer_map = HashMap::new();
    for LIRStatement { label, instruction } in lir {
        if instruction == LIRInstruction::Nop && label.is_none() {
            continue;
        }
        if let Some(label) = label {
            label_position_map.insert(label, result.len());
        }
        match instruction {
            LIRInstruction::LoadImmediateString(value) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::LoadStaticValue, static_value_generators.len() as u32));
                let value = Arc::new(RwLock::new(value)) as Arc<RwLock<dyn KirlVMValueCloneable>>;
                static_value_generators.push(Arc::new(move || Arc::clone(&value)));
            }
            LIRInstruction::LoadImmediateNumber(value) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::LoadStaticValue, static_value_generators.len() as u32));
                let value = Context::<Decimal128>::default().reduce(value);
                let value = Arc::new(RwLock::new(value)) as Arc<RwLock<dyn KirlVMValueCloneable>>;
                static_value_generators.push(Arc::new(move || Arc::clone(&value)));
            }
            LIRInstruction::LoadNamedValue(id) => result.push(KirlByteCode::new(KirlByteCodeOpcode::LoadStaticValue, *static_value_index.get(&id).expect("TODO:"))),
            LIRInstruction::Load(index) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::Load, index as u32));
            }
            LIRInstruction::Store(index) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::Store, index as u32));
            }
            LIRInstruction::JumpIfTrue(label) => {
                position_label_map.insert(result.len(), label);
                result.push(KirlByteCode::without_operand(KirlByteCodeOpcode::JumpIfTrue));
            }
            LIRInstruction::JumpIfHasType(ty, label) => {
                let type_map_len = type_map.len() as u32;
                let type_index = *type_map.entry(ty.into_normalized()).or_insert(type_map_len);
                result.push(KirlByteCode::new(KirlByteCodeOpcode::PushAdditionalOperand, type_index));
                position_label_map.insert(result.len(), label);
                result.push(KirlByteCode::without_operand(KirlByteCodeOpcode::JumpIfHasType));
            }
            LIRInstruction::Jump(label) => {
                position_label_map.insert(result.len(), label);
                result.push(KirlByteCode::without_operand(KirlByteCodeOpcode::Jump));
            }
            LIRInstruction::CallFunction(id) => match rust_function_index.get(&id) {
                None => {
                    function_pointer_map.insert(result.len(), id);
                    result.push(KirlByteCode::without_operand(KirlByteCodeOpcode::CallKirlFunction));
                }
                Some(index) => {
                    result.push(KirlByteCode::new(KirlByteCodeOpcode::CallRustFunction, *index));
                }
            },
            LIRInstruction::Return => {
                result.push(KirlByteCode::without_operand(KirlByteCodeOpcode::Return));
            }
            LIRInstruction::Nop => {
                result.push(KirlByteCode::without_operand(KirlByteCodeOpcode::Nop));
            }
            LIRInstruction::AccessTupleItem(index) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::AccessTupleItem, index as u32));
            }
            LIRInstruction::AccessMember(member) => {
                let member_name_map_len = member_name_map.len() as u32;
                let member = *member_name_map.entry(member).or_insert(member_name_map_len);
                result.push(KirlByteCode::new(KirlByteCodeOpcode::AccessMember, member));
            }
            LIRInstruction::AssignTupleItem(index) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::AssignTupleItem, index as u32));
            }
            LIRInstruction::AssignMember(member) => {
                let member_name_map_len = member_name_map.len() as u32;
                let member = *member_name_map.entry(member).or_insert(member_name_map_len);
                result.push(KirlByteCode::new(KirlByteCodeOpcode::AssignMember, member));
            }
            LIRInstruction::ConstructStruct(len) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::ConstructStruct, u32::try_from(len).unwrap()));
            }
            LIRInstruction::ConstructTuple(len) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::ConstructTuple, u32::try_from(len).unwrap()));
            }
            LIRInstruction::ConstructArray(len) => {
                result.push(KirlByteCode::new(KirlByteCodeOpcode::ConstructArray, u32::try_from(len).unwrap()));
            }
        }
    }
    for (position, label) in position_label_map {
        let target_position = label_position_map[&label] as isize;
        let diff = i32::try_from(target_position - position as isize).unwrap();
        let opcode = result[position].0;
        result[position] = KirlByteCode::new_signed(opcode, diff);
    }
    (result, function_pointer_map)
}
