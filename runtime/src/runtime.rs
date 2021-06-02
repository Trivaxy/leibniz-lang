use std::collections::HashMap;

use linked_hash_map::LinkedHashMap;
use num_complex::Complex64;

use crate::{
    function::{Function, NativeFunction},
    instruction::Instruction,
    value::Value,
};

use Value::*;

struct StackFrame {
    pos: usize,
    function: usize,
    locals: HashMap<usize, Value>,
    preserved_locals: HashMap<usize, Value>,
    stack: Vec<Value>,
}

impl StackFrame {
    fn new(pos: usize, function: usize, locals: HashMap<usize, Value>, stack: Vec<Value>) -> Self {
        StackFrame {
            pos,
            function,
            locals,
            preserved_locals: HashMap::new(),
            stack,
        }
    }
}

pub struct LeibnizRuntime {
    functions: HashMap<usize, Function>,
    native_functions: HashMap<usize, NativeFunction>,
    types: HashMap<usize, Vec<String>>,
    strings: HashMap<usize, String>,
    frames: Vec<StackFrame>,
    globals: HashMap<usize, Value>,
    preserved_globals: HashMap<usize, Value>,
    error: bool,
    error_message: Option<String>,
}

#[allow(mutable_borrow_reservation_conflict)]
impl LeibnizRuntime {
    pub fn new(
        functions: HashMap<usize, Function>,
        native_functions: HashMap<usize, NativeFunction>,
        types: HashMap<usize, Vec<String>>,
        strings: HashMap<usize, String>
    ) -> Self {
        let mut frames = Vec::with_capacity(functions.len());
        frames.push(StackFrame::new(0, 0, HashMap::new(), Vec::new()));
        frames.first_mut().unwrap().stack.push(Value::real(0.0));

        LeibnizRuntime {
            functions: functions,
            native_functions: native_functions,
            types: types,
            strings: strings,
            frames: frames,
            globals: HashMap::new(),
            preserved_globals: HashMap::new(),
            error: false,
            error_message: None,
        }
    }

    pub fn run(&mut self) -> Result<Value, String> {
        while self.current_frame().pos < self.current_func().len() {
            self.do_next_instruction();
            self.current_frame_mut().pos += 1;

            if self.error {
                return Err(self.error_message.as_ref().unwrap().clone());
            } else if !self.stack().is_empty() && self.stack().last().unwrap().is_empty() {
                return Err("invalid operation".into());
            }
        }

        let return_val = self.pop_value();
        self.frames.pop();

        if self.frames.is_empty() {
            return Ok(return_val);
        } else {
            self.push_value(return_val);
            self.current_frame_mut().pos += 1;
            return self.run();
        }
    }

    pub fn functions(&self) -> &HashMap<usize, Function> {
        &self.functions
    }

    pub fn native_functions(&self) -> &HashMap<usize, NativeFunction> {
        &self.native_functions
    }

    fn current_func(&self) -> &Function {
        &self.functions[&self.current_frame().function]
    }

    fn current_frame(&self) -> &StackFrame {
        &self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.frames.last_mut().unwrap()
    }

    fn stack(&mut self) -> &mut Vec<Value> {
        &mut self.current_frame_mut().stack
    }

    fn do_next_instruction(&mut self) {
        let instr = &self.current_func().instructions()[self.current_frame().pos];

        match instr {
            Instruction::PushNumber(n) => self.push_real(*n),
            Instruction::MakeImaginary => self.make_imaginary(),
            Instruction::MakeComplex => self.make_complex(),
            Instruction::Add => self.add(),
            Instruction::Subtract => self.subtract(),
            Instruction::Multiply => self.multiply(),
            Instruction::Divide => self.divide(),
            Instruction::Raise => self.raise(),
            Instruction::Remainder => self.remainder(),
            Instruction::Gamma => self.gamma(),
            Instruction::Equals => self.equals(),
            Instruction::LessThan => self.less_than(),
            Instruction::LessThanOrEquals => self.less_than_or_eq(),
            Instruction::GreaterThan => self.greater_than(),
            Instruction::GreaterThanOrEquals => self.greater_than_or_eq(),
            Instruction::Dupe => self.dupe(),
            Instruction::Index => self.index(),
            Instruction::SetLocal(index) => self.set_local(*index),
            Instruction::GetLocal(index) => self.get_local(*index),
            Instruction::SetGlobal(index) => self.set_global(*index),
            Instruction::GetGlobal(index) => self.get_global(*index),
            Instruction::PreserveLocal(index) => self.preserve_local(*index),
            Instruction::FetchLocal(index) => self.fetch_local(*index),
            Instruction::Call(index) => self.call(*index),
            Instruction::CallNative(index) => self.call_native(*index),
            Instruction::Jump(amount) => self.jump(*amount),
            Instruction::JumpFalse(amount) => self.jump_false(*amount),
            Instruction::JumpTrue(amount) => self.jump_true(*amount),
            Instruction::MakeArray(len) => self.make_array(*len),
            Instruction::MakeType(index) => self.make_type(*index),
            Instruction::LoadField(name) => self.load_field(&name.clone()), // TODO: figure out how to avoid this clone
            Instruction::LoadString(index) => self.load_string(*index)
        }
    }

    pub fn pop_f64(&mut self) -> f64 {
        self.stack().pop().unwrap().as_f64()
    }

    pub fn pop_complex(&mut self) -> Complex64 {
        self.stack().pop().unwrap().as_complex64()
    }

    pub fn pop_bool(&mut self) -> bool {
        let val = self.pop_complex();
        val.im == 0.0 && val.re != 0.0
    }

    pub fn pop_value(&mut self) -> Value {
        self.stack().pop().unwrap()
    }

    pub fn pop_values_reverse(&mut self, amount: usize) -> Vec<Value> {
        let stack = self.stack();
        stack.split_off(stack.len() - amount)
    }

    pub fn push_real(&mut self, n: f64) {
        self.stack().push(Value::real(n));
    }

    pub fn push_complex(&mut self, c: Complex64) {
        self.stack().push(Value::Complex(c));
    }

    pub fn push_bool(&mut self, b: bool) {
        if b {
            self.push_real(1.0);
        } else {
            self.push_real(0.0);
        }
    }

    pub fn push_value(&mut self, v: Value) {
        self.stack().push(v);
    }

    fn make_imaginary(&mut self) {
        let i = self.pop_f64();
        self.stack().push(Value::imaginary(i));
    }

    fn make_complex(&mut self) {
        let i = self.pop_f64();
        let r = self.pop_f64();
        self.stack().push(Value::complex(r, i));
    }

    fn add(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();
        self.push_value(val2 + val1);
    }

    fn subtract(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();
        self.push_value(val2 - val1);
    }

    fn multiply(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();
        self.push_value(val2 * val1);
    }

    fn divide(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();
        self.push_value(val2 / val1);
    }

    fn raise(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();
        self.push_value(val2.pow(val1));
    }

    fn remainder(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();
        self.push_value(val2.rem(val1));
    }

    fn gamma(&mut self) {
        let gamma = Value::calculate_gamma(self.pop_complex());

        if gamma.im == 0.0 {
            self.push_real(gamma.re);
        } else {
            self.push_complex(gamma);
        }
    }

    fn equals(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();

        self.push_bool(val1 == val2);
    }

    fn less_than(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();

        let result = match val2 {
            Real(n) => match val1 {
                Real(n2) => n < n2,
                Complex(c) => {
                    if c.im == 0.0 {
                        n < c.re
                    } else {
                        self.error(format!(
                            "cannot compare < between real value {} and complex value {}",
                            n, c
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare < between real value {} and a non-number",
                        n
                    ));
                    return;
                }
            },
            Complex(c) => match val1 {
                Real(n) => {
                    if c.im == 0.0 {
                        c.re < n
                    } else {
                        self.error(format!(
                            "cannot compare < between complex value {} and real value {}",
                            c, n
                        ));
                        return;
                    }
                }
                Complex(c2) => {
                    if c.im == 0.0 && c2.im == 0.0 {
                        c.re < c2.re
                    } else {
                        self.error(format!(
                            "cannot compare < between complex value {} and complex value {}",
                            c, c2
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare < between complex value {} and a non-number",
                        c
                    ));
                    return;
                }
            },
            _ => {
                self.error("cannot compare < between non-number values".to_string());
                return;
            }
        };

        self.push_bool(result);
    }

    fn less_than_or_eq(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();

        let result = match val2 {
            Real(n) => match val1 {
                Real(n2) => n <= n2,
                Complex(c) => {
                    if c.im == 0.0 {
                        n <= c.re
                    } else {
                        self.error(format!(
                            "cannot compare <= between real value {} and complex value {}",
                            n, c
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare <= between real value {} and a non-number",
                        n
                    ));
                    return;
                }
            },
            Complex(c) => match val1 {
                Real(n) => {
                    if c.im == 0.0 {
                        c.re <= n
                    } else {
                        self.error(format!(
                            "cannot compare <= between complex value {} and real value {}",
                            c, n
                        ));
                        return;
                    }
                }
                Complex(c2) => {
                    if c.im == 0.0 && c2.im == 0.0 {
                        c.re <= c2.re
                    } else {
                        self.error(format!(
                            "cannot compare <= between complex value {} and complex value {}",
                            c, c2
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare <= between complex value {} and a non-number",
                        c
                    ));
                    return;
                }
            },
            _ => {
                self.error("cannot compare <= between non-number values".to_string());
                return;
            }
        };

        self.push_bool(result);
    }

    fn greater_than(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();

        let result = match val2 {
            Real(n) => match val1 {
                Real(n2) => n > n2,
                Complex(c) => {
                    if c.im == 0.0 {
                        n > c.re
                    } else {
                        self.error(format!(
                            "cannot compare > between real value {} and complex value {}",
                            n, c
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare > between real value {} and a non-number",
                        n
                    ));
                    return;
                }
            },
            Complex(c) => match val1 {
                Real(n) => {
                    if c.im == 0.0 {
                        c.re > n
                    } else {
                        self.error(format!(
                            "cannot compare > between complex value {} and real value {}",
                            c, n
                        ));
                        return;
                    }
                }
                Complex(c2) => {
                    if c.im == 0.0 && c2.im == 0.0 {
                        c.re > c2.re
                    } else {
                        self.error(format!(
                            "cannot compare > between complex value {} and complex value {}",
                            c, c2
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare > between complex value {} and a non-number",
                        c
                    ));
                    return;
                }
            },
            _ => {
                self.error("cannot compare > between non-number values".to_string());
                return;
            }
        };

        self.push_bool(result);
    }

    fn greater_than_or_eq(&mut self) {
        let val1 = self.pop_value();
        let val2 = self.pop_value();

        let result = match val2 {
            Real(n) => match val1 {
                Real(n2) => n >= n2,
                Complex(c) => {
                    if c.im == 0.0 {
                        n >= c.re
                    } else {
                        self.error(format!(
                            "cannot compare >= between real value {} and complex value {}",
                            n, c
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare >= between real value {} and a non-number",
                        n
                    ));
                    return;
                }
            },
            Complex(c) => match val1 {
                Real(n) => {
                    if c.im == 0.0 {
                        c.re >= n
                    } else {
                        self.error(format!(
                            "cannot compare >= between complex value {} and real value {}",
                            c, n
                        ));
                        return;
                    }
                }
                Complex(c2) => {
                    if c.im == 0.0 && c2.im == 0.0 {
                        c.re >= c2.re
                    } else {
                        self.error(format!(
                            "cannot compare >= between complex value {} and complex value {}",
                            c, c2
                        ));
                        return;
                    }
                }
                _ => {
                    self.error(format!(
                        "cannot compare >= between complex value {} and a non-number",
                        c
                    ));
                    return;
                }
            },
            _ => {
                self.error("cannot compare < between non-number values".to_string());
                return;
            }
        };

        self.push_bool(result);
    }

    fn dupe(&mut self) {
        let val = self.stack().last().unwrap().clone();
        self.push_value(val);
    }

    fn index(&mut self) {
        let index = self.pop_f64();

        if index.fract() != 0.0 || index < 0.0 {
            self.error(format!(
                "tried to index array with invalid number {}",
                index
            ));
            return;
        }

        match self.pop_value() {
            Array(values) => match values.get(index as usize) {
                Some(value) => self.push_value(value.clone()),
                None => {
                    self.error(format!(
                        "tried to access array with out of bounds value: {}",
                        index
                    ));
                    return;
                }
            },
            _ => {
                self.error("tried to index a non-array value".to_string());
                return;
            }
        }
    }

    fn set_local(&mut self, index: usize) {
        let val = self.pop_value();
        self.current_frame_mut().locals.insert(index, val);
    }

    fn get_local(&mut self, index: usize) {
        let val = self.current_frame().locals.get(&index).unwrap();
        self.push_value(val.clone());
    }

    fn set_global(&mut self, index: usize) {
        let val = self.pop_value();
        self.globals.insert(index, val);
    }

    fn get_global(&mut self, index: usize) {
        let val = self.globals.get(&index).unwrap();
        self.push_value(val.clone());
    }

    fn preserve_local(&mut self, index: usize) {
        let frame = self.current_frame_mut();
        let val = frame.locals.get(&index).unwrap();
        frame.preserved_locals.insert(index, val.clone());
    }

    fn fetch_local(&mut self, index: usize) {
        let val = self
            .current_frame_mut()
            .preserved_locals
            .remove(&index)
            .unwrap();
        self.push_value(val);
    }

    fn call(&mut self, index: usize) {
        let parameter_names = self.functions().get(&index).unwrap().parameters();
        let parameter_count = parameter_names.len();
        let mut stack_values = self.pop_values_reverse(parameter_count);
        let mut parameters = HashMap::with_capacity(parameter_count);

        for i in (0..parameter_count).rev() {
            parameters.insert(i, stack_values.pop().unwrap());
        }

        let new_frame = StackFrame::new(0, index, parameters, Vec::new());
        self.frames.push(new_frame);
        self.do_next_instruction();
    }

    fn call_native(&mut self, index: usize) {
        let parameter_count = self.native_functions().get(&index).unwrap().parameters();
        let stack_values = self.pop_values_reverse(parameter_count);
        let func = self.native_functions.get_mut(&index).unwrap();

        (func.function)(&stack_values, self);
    }

    fn jump(&mut self, amount: isize) {
        let frame = self.current_frame_mut();
        frame.pos = if amount > 0 {
            frame.pos + amount as usize - 1
        } else {
            frame.pos - -amount as usize - 1
        }
    }

    fn jump_false(&mut self, amount: isize) {
        if !self.pop_bool() {
            self.jump(amount);
        }
    }

    fn jump_true(&mut self, amount: isize) {
        if self.pop_bool() {
            self.jump(amount);
        }
    }

    fn make_array(&mut self, len: usize) {
        let values = self.pop_values_reverse(len);
        self.push_value(Value::Array(values));
    }

    fn make_type(&mut self, index: usize) {
        let len = self.types.get(&index).unwrap().len();
        let values = self.pop_values_reverse(len);
        let r#type = self.types.get(&index).unwrap();
        let mut fields = LinkedHashMap::with_capacity(len);
        
        for i in 0..len {
            fields.insert(r#type[i].clone(), values[i].clone());
        }

        self.push_value(Value::Custom(fields));
    }

    fn load_field(&mut self, field: &str) {
        let val = self.pop_value();

        if !val.is_custom() {
            self.error(format!(
                "tried to load field '{}' from a value with no fields",
                field
            ));
            return;
        }

        let custom = val.as_custom();

        if !custom.contains_key(field) {
            self.error(format!("the field '{}' does not exist in the value", field));
            return;
        }

        self.push_value(custom.get(field).unwrap().clone());
    }

    fn load_string(&mut self, index: usize) {
        self.push_value(Value::LString(self.strings.get(&index).unwrap().clone()));
    }

    fn error(&mut self, error: String) {
        self.error = true;
        self.error_message = Some(format!(
            "runtime error in function '{}': {}",
            self.current_func().name,
            error
        ));
    }
}
