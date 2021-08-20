use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

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
    AccessMember,
    AssignMember,
    ConstructStruct,
    ConstructTuple,
    ConstructArray,
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
        assert_eq!(((operand >> 24) ^ (operand >> 16)) & 0xff, 0b1000_0000);
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
    fn argument_count(&self) -> usize;
    fn call(&mut self, args: Vec<Box<dyn KirlVMValueCloneable>>) -> Result<Box<dyn KirlVMValueCloneable>, Box<dyn Error>>;
}

pub struct FunctionWrapper<Args, Result, F>(F, PhantomData<(Args, Result)>);

macro_rules! count {
    ()=>{ 0 };
    ($t:ident)=>{ 1 };
    ($f:ident,$($t:ident),*)=>{ 1 + count!($($t),*) };
}

macro_rules! impl_fn {
    ()=>{
        impl<R, F> From<F> for FunctionWrapper<(), R, F>
            where F: FnMut() -> R {
            fn from(function: F) -> Self {
                FunctionWrapper(function, PhantomData::default())
            }
        }
        impl<R, E, F> KirlRustFunction for FunctionWrapper<(), Result<R, E>, F>
            where F: FnMut() -> Result<R, E>,
                  R: KirlVMValueCloneable,
                  E: std::error::Error + 'static,
                  Self: Send + Sync {
            fn argument_count(&self) -> usize { count!() }
            fn call(&mut self, args: Vec<Box<dyn KirlVMValueCloneable>>) -> Result<Box<dyn KirlVMValueCloneable>, Box<dyn Error>> {
                if let Ok([]) = <[_;count!()] as std::convert::TryFrom<_>>::try_from(args) {
                    self.0().map(|result|Box::new(result) as Box<dyn KirlVMValueCloneable>).map_err(|err|Box::new(err) as Box<dyn Error>)
                } else { unreachable!() }
            }
        }
    };
    ($($t:ident),*)=>{
        impl<R, F, $($t),*> From<F> for FunctionWrapper<($($t),*,), R, F>
            where F: FnMut($($t),*,) -> R {
            fn from(function: F) -> Self {
                FunctionWrapper(function, PhantomData::default())
            }
        }
        impl<R, E, F, $($t),*> KirlRustFunction for FunctionWrapper<($($t),*,), Result<R, E>, F>
            where F: FnMut($($t),*,) -> Result<R, E>,
                  R: KirlVMValueCloneable,
                  E: std::error::Error + 'static,
                  Self: Send + Sync,
                  $($t: KirlVMValueCloneable),* {
            fn argument_count(&self) -> usize { count!($($t),*) }
            fn call(&mut self, args: Vec<Box<dyn KirlVMValueCloneable>>) -> Result<Box<dyn KirlVMValueCloneable>, Box<dyn Error>> {
                #[allow(non_snake_case)]
                if let Ok([$($t),*]) = <[_;count!($($t),*)] as std::convert::TryFrom<_>>::try_from(args) {
                    self.0($(*downcast::<$t>($t).expect("")),*).map(|result|Box::new(result) as Box<dyn KirlVMValueCloneable>).map_err(|err|Box::new(err) as Box<dyn Error>)
                } else { unreachable!() }
            }
        }
    }
}

fn downcast<T: 'static>(value: Box<dyn KirlVMValueCloneable>) -> Result<Box<T>, Box<dyn KirlVMValueCloneable>> {
    let item = value.deref();
    if item.type_id() == TypeId::of::<T>() {
        unsafe {
            let ptr = Box::into_raw(value);
            Ok(Box::from_raw(ptr as *mut T))
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

pub trait KirlVMValue: Any + Debug + Send + Sync + 'static {}

impl KirlVMValue for String {}

impl KirlVMValue for () {}

impl KirlVMValue for LIRType {}

impl KirlVMValue for bool {}

impl KirlVMValue for Vec<Box<dyn KirlVMValueCloneable>> {}

impl KirlVMValue for HashMap<String, Box<dyn KirlVMValueCloneable>> {}

pub trait KirlVMValueCloneable: KirlVMValue {
    fn kirl_clone(&self) -> Box<dyn KirlVMValueCloneable>;
}

impl<T: KirlVMValue + Clone> KirlVMValueCloneable for T {
    fn kirl_clone(&self) -> Box<dyn KirlVMValueCloneable> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn KirlVMValueCloneable> {
    fn clone(&self) -> Self {
        self.kirl_clone()
    }
}

pub struct KirlVMExecutable {
    pub(crate) bytecodes: Vec<KirlByteCode>,
    pub(crate) entry_point: usize,
    pub(crate) static_value_generators: Vec<Arc<dyn Fn() -> Box<dyn KirlVMValueCloneable>>>,
    pub(crate) rust_functions: Vec<Arc<Mutex<dyn KirlRustFunction>>>,
    pub(crate) function_pointers: Vec<usize>,
    pub(crate) member_names: Vec<String>,
}

impl KirlVMExecutable {
    pub fn new(functions: impl IntoIterator<Item=(Uuid, Vec<LIRStatement>)>, static_value_generators: impl IntoIterator<Item=(Uuid, Arc<dyn Fn() -> Box<dyn KirlVMValueCloneable>>)>, rust_functions: impl IntoIterator<Item=(Uuid, Arc<Mutex<dyn KirlRustFunction>>)>, main_function: Uuid) -> Self {
        let (mut static_value_generators, static_value_index): (Vec<_>, HashMap<_, _>) = static_value_generators.into_iter().enumerate().map(|(i, (id, generator))| (generator, (id, u32::try_from(i).unwrap()))).unzip();
        let (rust_functions, rust_function_index): (Vec<_>, HashMap<_, _>) = rust_functions.into_iter().enumerate().map(|(i, (id, function))| (function, (id, u32::try_from(i).unwrap()))).unzip();
        let mut bytecodes = Vec::new();
        let mut function_pointers = HashMap::new();
        let mut member_name_map = HashMap::new();
        let mut function_references = HashMap::new();
        for (function_id, function_body) in functions {
            function_pointers.insert(function_id, bytecodes.len());
            let (bytecode, function_reference) = lir_to_bytecode(function_body, &mut member_name_map, &mut static_value_generators, &static_value_index, &rust_function_index);
            function_references.extend(function_reference.into_iter().map(|(index, id)| (index + bytecodes.len(), id)));
            bytecodes.extend(bytecode);
        }
        let (function_pointers, function_pointer_reference): (Vec<_>, HashMap<_, _>) = function_pointers.into_iter().enumerate().map(|(i, (id, pointer))| (pointer, (id, u32::try_from(i).unwrap()))).unzip();
        for (index, function) in function_references {
            let function_pointer = function_pointer_reference[&function];
            let opcode = bytecodes[index].0;
            bytecodes[index] = KirlByteCode::new(opcode, function_pointer);
        }
        let mut member_names = Vec::with_capacity(member_name_map.len());
        for (name, index) in member_name_map {
            member_names.resize_with(member_names.len().max(index as usize + 1), Default::default);
            member_names[index as usize] = name;
        }
        bytecodes.push(KirlByteCode::without_operand(KirlByteCodeOpcode::Return));
        KirlVMExecutable {
            bytecodes,
            entry_point: function_pointers[function_pointer_reference[&main_function] as usize],
            static_value_generators,
            rust_functions,
            function_pointers,
            member_names,
        }
    }
}

fn lir_to_bytecode(lir: impl IntoIterator<Item=LIRStatement>, member_name_map: &mut HashMap<String, u32>, static_value_generators: &mut Vec<Arc<dyn Fn() -> Box<dyn KirlVMValueCloneable>>>, static_value_index: &HashMap<Uuid, u32>, rust_function_index: &HashMap<Uuid, u32>) -> (impl IntoIterator<Item=KirlByteCode>, impl IntoIterator<Item=(usize, Uuid)>) {
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
                static_value_generators.push(Arc::new(move || Box::new(value.clone())));
            }
            LIRInstruction::LoadImmediateNumber(_) => {
                todo!()
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
                result.push(KirlByteCode::new(KirlByteCodeOpcode::LoadStaticValue, static_value_generators.len() as u32));
                static_value_generators.push(Arc::new(move || Box::new(ty.clone())));
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
            LIRInstruction::AccessMember(member) => {
                let member_name_map_len = member_name_map.len() as u32;
                let member = *member_name_map.entry(member).or_insert(member_name_map_len);
                result.push(KirlByteCode::new(KirlByteCodeOpcode::AccessMember, member));
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
