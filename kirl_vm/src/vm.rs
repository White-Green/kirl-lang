use kirl_common::interface::{InterchangeKirlVMValue, KirlVMValueLock};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::bytecode::{KirlByteCodeOpcode, KirlVMExecutable};

fn unwrap<T: Clone>(ptr: Arc<RwLock<T>>) -> T {
    Arc::try_unwrap(ptr).map(|rw| rw.into_inner().expect("")).unwrap_or_else(|ptr| ptr.read().expect("").clone())
}

pub fn exec(
    KirlVMExecutable {
        bytecodes,
        entry_point,
        static_value_generators,
        rust_functions,
        function_pointers,
        member_names,
        types,
    }: &KirlVMExecutable,
) {
    let mut local_stack = Vec::new();
    let mut global_stack: Vec<Arc<dyn KirlVMValueLock>> = Vec::new();
    let mut global_stack_offset = 0usize;
    let mut global_stack_offset_stack = Vec::new();
    let mut program_counter_stack = Vec::new();
    let mut program_counter = *entry_point;
    let mut additional_operand = Vec::new();
    loop {
        let instruction = bytecodes[program_counter];
        match instruction.opcode() {
            KirlByteCodeOpcode::LoadStaticValue => {
                let operand = instruction.operand();
                local_stack.push(static_value_generators[operand as usize]());
            }
            KirlByteCodeOpcode::Load => {
                let operand = instruction.operand() as usize + global_stack_offset;
                local_stack.push(global_stack[operand].clone());
            }
            KirlByteCodeOpcode::Store => {
                let operand = instruction.operand() as usize + global_stack_offset;
                if global_stack.len() <= operand {
                    global_stack.resize_with(operand + 1, || Arc::new(RwLock::new(Vec::new().into_boxed_slice())));
                }
                global_stack[operand] = local_stack.pop().expect("");
            }
            KirlByteCodeOpcode::JumpIfTrue => {
                let condition = local_stack.pop().expect("");
                let condition = bool::try_from_kirl_value(condition).expect("");
                if *condition.read().expect("") {
                    let operand = instruction.operand_signed();
                    program_counter = ((program_counter as isize) + (operand as isize)) as usize;
                    continue;
                }
            }
            KirlByteCodeOpcode::JumpIfHasType => {
                let ty = &types[additional_operand.pop().expect("") as usize];
                let condition = local_stack.last().expect("");
                if condition.get_type().is_a(ty) {
                    let operand = instruction.operand_signed();
                    program_counter = ((program_counter as isize) + (operand as isize)) as usize;
                    continue;
                }
            }
            KirlByteCodeOpcode::Jump => {
                let operand = instruction.operand_signed();
                program_counter = ((program_counter as isize) + (operand as isize)) as usize;
                continue;
            }
            KirlByteCodeOpcode::CallKirlFunction => {
                let operand = instruction.operand();
                program_counter_stack.push(program_counter);
                program_counter = function_pointers[operand as usize];
                global_stack_offset_stack.push(global_stack_offset);
                global_stack_offset = global_stack.len();
                continue;
            }
            KirlByteCodeOpcode::CallRustFunction => {
                let operand = instruction.operand();
                let mut function = rust_functions[operand as usize].lock().unwrap();
                let argument_count = function.argument_count();
                let mut arguments = Vec::with_capacity(argument_count);
                for _ in 0..argument_count {
                    arguments.push(local_stack.pop().expect(""));
                }
                let ret = function.call(arguments).expect("TODO:組み込み関数が失敗した場合(ここ)の処理");
                local_stack.push(ret);
            }
            KirlByteCodeOpcode::Return => {
                if program_counter_stack.is_empty() {
                    return;
                }
                program_counter = program_counter_stack.pop().expect("");
                global_stack_offset = global_stack_offset_stack.pop().expect("");
            }
            KirlByteCodeOpcode::Nop => {}
            KirlByteCodeOpcode::AccessTupleItem => {
                let operand = instruction.operand();
                let value = local_stack.pop().expect("");
                let value = Box::<[Arc<dyn KirlVMValueLock>]>::try_from_kirl_value(value).expect("");
                let value = value.read().expect("");
                let value = Arc::clone(&value[operand as usize]);
                local_stack.push(value);
            }
            KirlByteCodeOpcode::AccessMember => {
                let operand = instruction.operand();
                let value = local_stack.pop().expect("");
                let value = HashMap::<String, Arc<dyn KirlVMValueLock>>::try_from_kirl_value(value).expect("");
                let value = value.read().expect("");
                let member = value.get(&member_names[operand as usize]).expect("");
                local_stack.push(Arc::clone(member));
            }
            KirlByteCodeOpcode::AssignTupleItem => {
                let operand = instruction.operand();
                let value = local_stack.pop().expect("");
                let dest = local_stack.pop().expect("");
                let dest = Box::<[Arc<dyn KirlVMValueLock>]>::try_from_kirl_value(dest).expect("");
                dest.write().expect("")[operand as usize] = value;
            }
            KirlByteCodeOpcode::AssignMember => {
                let operand = instruction.operand();
                let value = local_stack.pop().expect("");
                let dest = local_stack.pop().expect("");
                let dest = HashMap::<String, Arc<dyn KirlVMValueLock>>::try_from_kirl_value(dest).expect("");
                dest.write().expect("").insert(member_names[operand as usize].clone(), value);
            }
            KirlByteCodeOpcode::ConstructStruct => {
                let operand = instruction.operand();
                let mut result = HashMap::with_capacity(operand as usize);
                for _ in 0..operand {
                    let name = local_stack.pop().expect("");
                    let name = unwrap(<Box<str>>::try_from_kirl_value(name).expect(""));
                    let value = local_stack.pop().expect("");
                    result.insert(name.into_string(), value);
                }
                local_stack.push(Arc::new(RwLock::new(result)));
            }
            KirlByteCodeOpcode::ConstructTuple => {
                let operand = instruction.operand();
                let mut result = Vec::with_capacity(operand as usize);
                for _ in 0..operand {
                    result.push(local_stack.pop().expect(""));
                }
                local_stack.push(Arc::new(RwLock::new(result.into_boxed_slice())));
            }
            KirlByteCodeOpcode::ConstructArray => {
                let operand = instruction.operand();
                let mut result = Vec::with_capacity(operand as usize);
                for _ in 0..operand {
                    result.push(local_stack.pop().expect(""));
                }
                local_stack.push(Arc::new(RwLock::new(result)));
            }
            KirlByteCodeOpcode::PushAdditionalOperand => {
                let operand = instruction.operand();
                additional_operand.push(operand);
            }
        }
        program_counter += 1;
    }
}
