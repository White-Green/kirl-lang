use std::any::Any;
use std::collections::HashMap;

use crate::bytecode::{KirlByteCode, KirlByteCodeOpcode, KirlRustFunction, KirlVMExecutable, KirlVMValueCloneable};

pub fn exec(
    KirlVMExecutable {
        bytecodes,
        entry_point,
        static_value_generators,
        rust_functions,
        function_pointers,
        member_names,
    }: &mut KirlVMExecutable,
) {
    fn exec_inner(bytecodes: &[KirlByteCode], start: usize, local_stack: &mut Vec<Box<dyn KirlVMValueCloneable>>, static_value_generators: &mut [Box<dyn FnMut() -> Box<dyn KirlVMValueCloneable>>], rust_functions: &mut [Box<dyn KirlRustFunction>], function_pointers: &mut [usize], member_names: &mut [String]) {
        let mut global_stack: Vec<Box<dyn KirlVMValueCloneable>> = Vec::new();
        let mut program_counter = start;
        loop {
            let instruction = bytecodes[program_counter];
            match instruction.opcode() {
                KirlByteCodeOpcode::LoadStaticValue => {
                    let operand = instruction.operand();
                    local_stack.push(static_value_generators[operand as usize]());
                }
                KirlByteCodeOpcode::Load => {
                    let operand = instruction.operand() as usize;
                    local_stack.push(global_stack[operand].kirl_clone());
                }
                KirlByteCodeOpcode::Store => {
                    let operand = instruction.operand() as usize;
                    if global_stack.len() <= operand {
                        global_stack.resize_with(operand + 1, || Box::new(String::new()));
                    }
                    global_stack[operand] = local_stack.pop().expect("");
                }
                KirlByteCodeOpcode::JumpIfTrue => {
                    let condition = local_stack.pop().expect("");
                    let condition = *(&condition as &dyn Any).downcast_ref::<bool>().expect("");
                    if condition {
                        let operand = instruction.operand_signed();
                        program_counter = ((program_counter as isize) + (operand as isize)) as usize;
                        continue;
                    }
                }
                KirlByteCodeOpcode::JumpIfHasType => {
                    todo!()
                }
                KirlByteCodeOpcode::Jump => {
                    let operand = instruction.operand_signed();
                    program_counter = ((program_counter as isize) + (operand as isize)) as usize;
                }
                KirlByteCodeOpcode::CallKirlFunction => {
                    let operand = instruction.operand();
                    exec_inner(bytecodes, function_pointers[operand as usize], local_stack, static_value_generators, rust_functions, function_pointers, member_names)
                }
                KirlByteCodeOpcode::CallRustFunction => {
                    let operand = instruction.operand();
                    let function = &mut rust_functions[operand as usize];
                    let argument_count = function.argument_count();
                    let mut arguments = Vec::with_capacity(argument_count);
                    for _ in 0..argument_count {
                        arguments.push(local_stack.pop().expect(""));
                    }
                    let ret = function.call(&arguments);
                    local_stack.push(ret);
                }
                KirlByteCodeOpcode::Return => {
                    return;
                }
                KirlByteCodeOpcode::Nop => {}
                KirlByteCodeOpcode::AccessMember => {
                    let operand = instruction.operand();
                    let value = local_stack.pop().expect("");
                    let value = (&value as &dyn Any).downcast_ref::<HashMap<String, Box<dyn KirlVMValueCloneable>>>().expect("");
                    let member = value.get(&member_names[operand as usize]).expect("");
                    local_stack.push(member.kirl_clone());
                }
                KirlByteCodeOpcode::AssignMember => {
                    let operand = instruction.operand();
                    let value = local_stack.pop().expect("");
                    let mut dest = local_stack.pop().expect("");
                    let dest = (&mut dest as &mut dyn Any).downcast_mut::<HashMap<String, Box<dyn KirlVMValueCloneable>>>().expect("");
                    dest.insert(member_names[operand as usize].clone(), value);
                }
                KirlByteCodeOpcode::ConstructStruct => {
                    let operand = instruction.operand();
                    let mut result = HashMap::with_capacity(operand as usize);
                    for _ in 0..operand {
                        let name = local_stack.pop().expect("");
                        let name = (&name as &dyn Any).downcast_ref::<String>().expect("").clone();
                        let value = local_stack.pop().expect("");
                        result.insert(name, value);
                    }
                    local_stack.push(Box::new(result));
                }
                KirlByteCodeOpcode::ConstructTuple => {
                    let operand = instruction.operand();
                    let mut result = HashMap::with_capacity(operand as usize);
                    for i in 0..operand {
                        result.insert(format!("{}", i), local_stack.pop().expect(""));
                    }
                    local_stack.push(Box::new(result));
                }
                KirlByteCodeOpcode::ConstructArray => {
                    let operand = instruction.operand();
                    let mut result = Vec::with_capacity(operand as usize);
                    for _ in 0..operand {
                        result.push(local_stack.pop().expect(""));
                    }
                    local_stack.push(Box::new(result));
                }
            }
            program_counter += 1;
        }
    }
    let mut local_stack = Vec::new();
    exec_inner(bytecodes, *entry_point, &mut local_stack, static_value_generators, rust_functions, function_pointers, member_names)
}
