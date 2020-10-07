use std::{collections::HashMap, fmt};
use std::ops;
use crate::parser::{Operator, ParserNode};
use Value::*;

pub enum Value {
    Number(f64),
    Vector(f64, f64)
}

type ValueOutput = Result<Value, String>;

impl ops::Add<Value> for Value {
    type Output = ValueOutput;

    fn add(self, rhs: Value) -> Self::Output {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(Number(n + n2)),
                Vector(_, _) => Err("cannot add a number to a vector".into())
            },
            Vector(x, y) => match rhs {
                Number(_) => Err("cannot add a vector to a number".into()),
                Vector(x2, y2) => Ok(Vector(x + x2, y + y2))
            }
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = ValueOutput;

    fn sub(self, rhs: Value) -> Self::Output {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(Number(n - n2)),
                Vector(_, _) => Err("cannot subtract a number from a vector".into())
            },
            Vector(x, y) => match rhs {
                Number(_) => Err("cannot subtract a vector from a number".into()),
                Vector(x2, y2) => Ok(Vector(x - x2, y - y2))
            }
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = ValueOutput;

    fn mul(self, rhs: Value) -> Self::Output {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(Number(n * n2)),
                Vector(x, y) => Ok(Vector(x * n, y * n))
            },
            Vector(x, y) => match rhs {
                Number(n) => Ok(Vector(x * n, y * n)),
                Vector(_, _) => Err("cannot multiply a vector with a vector. use dot(vector, vector) or cross(vector, vector) instead".into())
            }
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = ValueOutput;

    fn div(self, rhs: Value) -> Self::Output {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(Number(n / n2)),
                Vector(x, y) => Ok(Vector(x / n, y / n))
            },
            Vector(x, y) => match rhs {
                Number(n) => Ok(Vector(x / n, y / n)),
                Vector(_, _) => Err("cannot divide a vector by another vector".into())
            }
        }
    }
}

impl ops::Rem<Value> for Value {
    type Output = ValueOutput;

    fn rem(self, rhs: Value) -> Self::Output {
        use Value::*;

        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(Number(n % n2)),
                Vector(_, _) => Err("cannot find remainder between number and vector".into())
            },
            Vector(_, _) => match rhs {
                Number(_) => Err("cannot find remainder between vector and number".into()),
                Vector(_, _) => Err("cannot find remainder between vector and vector".into())
            }
        }
    }
}

// here are operators not overloadable in rust
impl Value {
    fn pow(self, rhs: Value) -> ValueOutput {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(Number(n.powf(n2))),
                Vector(_, _) => Err("cannot raise a vector to a numeric power".into())
            },
            Vector(x, y) => match rhs {
                Number(n) => Ok(Vector(x.powf(n), y.powf(n))),
                Vector(_, _) => Err("cannot raise a vector to a vector power".into())
            }
        }
    }

    fn greater_than(self, rhs: Value) -> ValueOutput {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(if n > n2 {Number(1.0)} else {Number(0.0)}),
                Vector(_, _) => Err("cannot compare greater-than between a number and vector".into())
            },
            Vector(_, _) => match rhs {
                Number(_) => Err("cannot compare greater-than between a vector and a number".into()),
                Vector(_, _) => Err("cannot compare greater-than between a vector and a vector".into())
            }
        }
    }

    fn less_than(self, rhs: Value) -> ValueOutput {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(if n < n2 {Number(1.0)} else {Number(0.0)}),
                Vector(_, _) => Err("cannot compare less-than between a number and vector".into())
            },
            Vector(_, _) => match rhs {
                Number(_) => Err("cannot compare less-than between a vector and a number".into()),
                Vector(_, _) => Err("cannot compare less-than between a vector and a vector".into())
            }
        }
    }

    fn greater_than_or_equals(self, rhs: Value) -> ValueOutput {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(if n >= n2 {Number(1.0)} else {Number(0.0)}),
                Vector(_, _) => Err("cannot compare greater-than-or-equals between a number and vector".into())
            },
            Vector(_, _) => match rhs {
                Number(_) => Err("cannot compare greater-than-or-equals between a vector and a number".into()),
                Vector(_, _) => Err("cannot compare greater-than-or-equals between a vector and a vector".into())
            }
        }
    }

    fn less_than_or_equals(self, rhs: Value) -> ValueOutput {
        match self {
            Number(n) => match rhs {
                Number(n2) => Ok(if n <= n2 {Number(1.0)} else {Number(0.0)}),
                Vector(_, _) => Err("cannot compare greater-than-or-equals between a number and vector".into())
            },
            Vector(_, _) => match rhs {
                Number(_) => Err("cannot compare greater-than-or-equals between a vector and a number".into()),
                Vector(_, _) => Err("cannot compare greater-than-or-equals between a vector and a vector".into())
            }
        }
    }

    fn expect_number<'a>(&self, message: &'a str) -> Result<f64, &'a str> {
        match self {
            Number(n) => Ok(*n),
            _ => Err(message),
        }
    }

    fn expect_vec<'a>(&self, message: &'a str) -> Result<(f64, f64), &'a str> {
        match self {
            Vector(x, y) => Ok((*x, *y)),
            _ => Err(message)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number(n) => write!(f, "{}", n),
            Vector(x, y) => write!(f, "({}, {})", x, y)
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Number(n) => Number(*n),
            Vector(x, y) => Vector(*x, *y)
        }
    }
}

struct RuntimeState<'a> {
    globals: HashMap<&'a str, Value>,
    locals: HashMap<&'a str, Value>,
    functions: HashMap<&'a str, &'a ParserNode<'a>>,
    builtin_functions: HashMap<&'a str, BuiltinFunction>
}

struct BuiltinFunction {
    parameter_count: usize,
    body: fn (&[Value]) -> Result<Value, String>
}

impl BuiltinFunction {
    fn new(params: usize, closure: fn (&[Value]) -> Result<Value, String>) -> Self {
        BuiltinFunction {
            parameter_count: params,
            body: closure
        }
    }
}

impl<'a> RuntimeState<'a> {
    fn new() -> Self {
        RuntimeState { 
            globals: HashMap::new(),
            locals: HashMap::new(),
            functions: HashMap::new(),
            builtin_functions: HashMap::new()
        }
    }

    fn add_default_globals_and_functions(&mut self) {
        self.add_global("pi", Value::Number(std::f64::consts::PI));

        self.add_builtin("vec", BuiltinFunction::new(
            2,
            |params| {
                let x = params[0].expect_number("the x component of the vector is not a number")?;
                let y = params[1].expect_number("the x component of the vector is not a number")?;
                Ok(Vector(x, y))
            }));

        self.add_builtin("x", BuiltinFunction::new(
            1,
            |params| {
                let vec = params[0].expect_vec("expected vector to take x component out of")?;
                Ok(Number(vec.0))
            }));

        self.add_builtin("y", BuiltinFunction::new(
            1,
            |params| {
                let vec = params[0].expect_vec("expected vector to take y component out of")?;
                Ok(Number(vec.1))
            }));

        self.add_builtin("y", BuiltinFunction::new(
            1,
            |params| {
                let vec = params[0].expect_vec("expected vector to take y component out of")?;
                Ok(Number(vec.1))
            }));

        self.add_builtin("sin", BuiltinFunction::new(
            1,
            |params| {
                let num = params[0].expect_number("expected number to find sine of")?;
                Ok(Number(num.sin()))
            }));

        self.add_builtin("cos", BuiltinFunction::new(
            1,
            |params| {
                let num = params[0].expect_number("expected number to find cosine of")?;
                Ok(Number(num.cos()))
            }));

        self.add_builtin("tan", BuiltinFunction::new(
            1,
            |params| {
                let num = params[0].expect_number("expected number to find tangent of")?;
                Ok(Number(num.tan()))
            }));

        self.add_builtin("print", BuiltinFunction::new(
            1,
            |params| {
                println!("{}", params[0]);
                Ok(params[0].clone())
            }));
    }

    fn add_global(&mut self, name: &'a str, value: Value) {
        self.globals.insert(name, value);
    }

    fn add_local(&mut self, name: &'a str, value: Value) {
        self.locals.insert(name, value);
    }

    fn remove_local(&mut self, name: &'a str) {
        self.locals.remove(name);
    }

    fn has_local(&self, name: &'a str) -> bool {
        self.locals.contains_key(name)
    }

    fn has_global(&self, name: &'a str) -> bool {
        self.globals.contains_key(name)
    }

    fn add_function(&mut self, name: &'a str, body: &'a ParserNode) {
        self.functions.insert(name, body);
    }

    fn add_builtin(&mut self, name: &'a str, function: BuiltinFunction) {
        self.builtin_functions.insert(name, function);
    }

    fn has_function(&self, name: &'a str) -> bool {
        self.functions.contains_key(name) || self.builtin_functions.contains_key(name)
    }

    fn evaluate(&mut self, node: &'a ParserNode<'a>) -> Result<Value, String> {
        match node {
            ParserNode::Number(num) => Ok(Number(*num)),
            ParserNode::Identifier(identifier) => {
                if self.has_local(identifier) {
                    Ok(self.locals[identifier].clone())
                }
                else if self.has_global(identifier) {
                    Ok(self.globals[identifier].clone())
                }
                else {
                    return Err(format!("unknown variable: {}", identifier));
                }
            },
            ParserNode::Operation(left, operator, right) => {
                let left = self.evaluate(&*left)?;
                let right = self.evaluate(&*right)?;

                Ok(match operator {
                    Operator::Add => (left + right)?,
                    Operator::Subtract => (left - right)?,
                    Operator::Multiply => (left * right)?,
                    Operator::Divide => (left / right)?,
                    Operator::Power => left.pow(right)?,
                    Operator::Modulo => (left % right)?,
                    Operator::GreaterThan => left.greater_than(right)?,
                    Operator::LessThan => left.less_than(right)?,
                    Operator::GreaterThanOrEquals => left.greater_than_or_equals(right)?,
                    Operator::LessThanOrEquals => left.less_than_or_equals(right)?
                })
            },
            ParserNode::FunctionCall(name, arguments) => {
                if !self.has_function(name) {
                    return Err(format!("unknown function: {}", name));
                }

                if self.builtin_functions.contains_key(name) {
                    if arguments.len() != self.builtin_functions[name].parameter_count {
                        return Err(format!("{} expects {} parameters, but only {} were supplied", name, self.builtin_functions[name].parameter_count, arguments.len()));
                    }

                    let evaluated_arguments: Vec<Result<Value, String>> = arguments.iter()
                        .map(|argument| self.evaluate(argument))
                        .collect();

                    for argument in evaluated_arguments.iter() {
                        if let Err(_) = argument {
                            return argument.clone();
                        }
                    }

                    let evaluated_arguments: Vec<Value> = evaluated_arguments.into_iter()
                        .map(|argument| argument.unwrap())
                        .collect();

                    return (self.builtin_functions[name].body)(&evaluated_arguments);
                }

                let mut functions = self.functions.iter()
                    .filter(|function| *function.0 == *name)
                    .map(|function| *function.1)
                    .collect::<Vec<&ParserNode>>();

                if let ParserNode::FunctionDeclaration(_, parameters, body) = functions.pop().unwrap() {
                    if arguments.len() != parameters.len() {
                        return Err(format!("{} expects {} parameters, but only {} were supplied", name, parameters.len(), arguments.len()));
                    }

                    let mut preserved_locals = HashMap::new();

                    for parameter in parameters.iter() {
                        if self.has_local(parameter) {
                            preserved_locals.insert(*parameter, self.locals[parameter].clone());
                        }
                    }

                    for i in 0..arguments.len() {
                        let arg_value = self.evaluate(&arguments[i])?;
                        self.add_local(parameters[i], arg_value);
                    }
    
                    let result = self.evaluate(&*body)?;

                    for i in 0..parameters.len() {
                        let parameter = parameters[i];

                        if !preserved_locals.contains_key(parameter) {
                            self.remove_local(parameter);
                        }
                        else {
                            self.add_local(parameter, preserved_locals[parameter].clone());
                        }
                    }
    
                    return Ok(result);
                }
                else {
                    unreachable!()
                }
            },
            ParserNode::Conditional(predicate, true_expr, false_expr) => {
                let predicate = self.evaluate(&*predicate)?.expect_number("a predicate to a conditional expression must be a number")?;
                
                if predicate != 0.0 {
                    return Ok(self.evaluate(&*true_expr)?);
                }
                else {
                    return Ok(self.evaluate(&*false_expr)?);
                }
            },
            ParserNode::FunctionDeclaration(name, _, _) => {
                if self.has_function(name) {
                    return Err(format!("redeclared a function that already is defined: {}", name));
                }
    
                self.add_function(name, node);
    
                Ok(Number(0.0))
            },
            ParserNode::VariableDeclaration(name, expression) => {
                if self.has_local(name) {
                    return Err(format!("you cannot redeclare a variable: {}", name));
                }

                let value = self.evaluate(&*expression)?;
                self.add_local(name, value);
    
                Ok(Number(0.0))
            },
            ParserNode::Loop(parameter, range, body) => {
                if let ParserNode::Range(first, second, step) = &**range /* :S */ {
                    let first_bound = self.evaluate(&*first)?.expect_number("the first bound must be a number")?;
                    let second_bound = self.evaluate(&*second)?.expect_number("the second bound must be a number")?;
                    let step = self.evaluate(&*step)?.expect_number("the step must be a number")?;

                    if step <= 0.0 {
                        return Err("a step cannot be less than or equal to 0".to_string());
                    }
    
                    let mut x = first_bound;
                    let mut last_evaluated = Number(0.0);

                    if first_bound < second_bound {
                        while x < second_bound {
                            self.add_local(parameter, Number(x));
                            last_evaluated = (last_evaluated+ self.evaluate(&*body)?)?;
                            
                            if x + step >= second_bound {
                                x = second_bound;
                                self.add_local(parameter, Number(x));
                                last_evaluated = (last_evaluated+ self.evaluate(&*body)?)?;
                            }
                            else {
                                x += step;
                            }
                        }
                    }
                    else if first_bound > second_bound {
                        while x > second_bound {
                            self.add_local(parameter, Number(x));
                            last_evaluated = (last_evaluated+ self.evaluate(&*body)?)?;
                            
                            if x - step <= second_bound {
                                x = second_bound;
                                self.add_local(parameter, Number(x));
                                last_evaluated = (last_evaluated+ self.evaluate(&*body)?)?;
                            }
                            else {
                                x -= step;
                            }
                        }
                    }

                    self.remove_local(parameter);
    
                    return Ok(last_evaluated);
                }
                else {
                    unreachable!()
                }
            },
            ParserNode::Assignment(identifiers, expression) => {
                let expression = self.evaluate(expression)?;

                for identifier in identifiers.iter() {
                    if !self.has_local(identifier) && !self.has_global(identifier) {
                        return Err(format!("use of undefined variable: {}", identifier));
                    }

                    self.add_local(identifier, expression.clone());
                }

                Ok(expression)
            },
            ParserNode::Tree(nodes) => {
                if nodes.is_empty() {
                    return Ok(Number(0.0));
                }

                match nodes.last().unwrap() {
                    ParserNode::VariableDeclaration(_, _) | ParserNode ::FunctionDeclaration(_, _, _) => Err("a tree must end with an expression".to_owned()),
                    _ => {
                        let mut last_evaluated = Value::Number(0.0);
                        let mut new_locals = Vec::new();
    
                        for node in nodes.into_iter() {
                            last_evaluated = self.evaluate(node)?;
    
                            if let ParserNode::VariableDeclaration(name, _) = node {
                                new_locals.push(name);
                            }
                            else if let ParserNode::FunctionDeclaration(name, _, _) = node {
                                new_locals.push(name);
                            }
                        }
    
                        for local in new_locals {
                            self.remove_local(local);
                        }

                        return Ok(last_evaluated);
                    }
                }
            }
            _ => unreachable!()
        }
    }
}

pub fn execute(root: ParserNode) -> Result<Value, String> {
    let mut runtime = RuntimeState::new();
    runtime.add_default_globals_and_functions();
    runtime.evaluate(&root)
}