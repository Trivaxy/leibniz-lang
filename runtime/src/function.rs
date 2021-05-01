use crate::{instruction::Instruction, runtime::LeibnizRuntime, value::Value};

pub struct Function {
    pub name: String,
    parameters: Vec<String>,
    body: Vec<Instruction>,
    cursor: usize,
}

impl Function {
    pub fn new(name: String, parameters: Vec<String>) -> Self {
        Function {
            name: name,
            parameters: parameters,
            body: Vec::new(),
            cursor: 0,
        }
    }

    pub fn emit_instr(&mut self, instruction: Instruction) {
        self.body.insert(self.cursor, instruction);
        self.cursor += 1;
    }

    pub fn remove_instr(&mut self) {
        self.body.remove(self.cursor);
    }

    pub fn remove_instrs(&mut self, amount: usize) {
        self.body.drain(self.cursor + 1 - amount..self.cursor + 1);
        self.cursor -= 2;
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.body
    }

    pub fn parameters(&self) -> &[String] {
        &self.parameters
    }

    pub fn len(&self) -> usize {
        self.instructions().len()
    }

    pub fn move_cursor_to_end(&mut self) {
        self.cursor = self.body.len();
    }

    pub fn move_cursor_to(&mut self, index: usize) {
        self.cursor = index;
    }

    pub fn cursor_pos(&self) -> usize {
        self.cursor
    }
}

pub type NFunction = fn(&[Value], &mut LeibnizRuntime);

pub struct NativeFunction {
    pub function: NFunction,
    parameter_count: usize,
}

impl NativeFunction {
    pub fn new(function: NFunction, parameter_count: usize) -> Self {
        NativeFunction {
            function,
            parameter_count,
        }
    }

    pub fn parameters(&self) -> usize {
        self.parameter_count
    }
}
