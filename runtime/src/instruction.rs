use std::fmt::{Debug, Display};

// Every possible instruction that LBZVM can run
// You will find the term "numerical object" used a lot here. A numerical object is either an f64 or a complex object
// For the sake of being clear, the topmost value on the stack is referred to as A, beneath it is B, beneath that is C, and so on
// All binary operations put B first then A, basically you'll get B [OPERATION] A
#[derive(Clone)]
pub enum Instruction {
    PushNumber(f64),      // Pushes an f64 onto the stack
    MakeImaginary,        // Pops an f64 off the stack and returns Complex(0, A)
    MakeComplex,          // Pops A and B off the stack and returns Complex(B, A)
    Add,                  // Pops 2 numerical objects off the stack and pushes their sum
    Subtract,             // Pops 2 numerical objects off the stack and pushes their difference
    Multiply,             // Pops 2 numerical objects off the stack and pushes their product
    Divide,               // Pops 2 numerical objects off the stack and pushes their quotient
    Raise,                // Pops 2 numerical objects off the stack and pushes their power
    Remainder,            // Pops 2 numerical objects off the stack and pushes their remainder
    Gamma,                // Pops a numerical object off the stack and pushes its gamma function
    Equals,               // Pops 2 values off the stack and checks if they are equal
    LessThan, // Pops 2 f64 off the stack, and checks if B < A. If true, 1 is pushed, otherwise 0
    LessThanOrEquals, // Pops 2 f64 off the stack, and checks if B <= A. If true, 1 is pushed, otherwise 0
    GreaterThan, // Pops 2 f64 off the stack, and checks if B > A. If true, 1 is pushed, otherwise 0
    GreaterThanOrEquals, // Pops 2 f64 off the stack, and checks if B >= A. If true, 1 is pushed, otherwise 0
    Dupe,                // Duplicates the value on top of the stack and pushes it
    Index, // Pops A and B off the stack where A is an f64 and B is an array and pushes the value of B[A]
    SetLocal(usize), // Pops a value off the stack and stores it in the local at the specified index
    GetLocal(usize), // Pushes the value of the local at the specified index onto the stack
    SetGlobal(usize), // Pops a value off the stack and stores it in the global at the specified index
    GetGlobal(usize), // Pushes the value of the global at the specified index onto the stack
    PreserveLocal(usize), // Copies and stores the value of the local at the specified index somewhere off-stack
    FetchLocal(usize), // Retrieves the value of the preserved local at the specified index from off-stack, pushes it onto the stack and deletes the off-stack value
    Call(usize), // Calls the function at the specified index, popping the number of parameters it needs off the stack, reversing them and supplying them. The return value of the function is pushed onto the stack
    CallNative(usize), // Calls the native function at the specified index, popping the number of parameters it needs off the stack, reversing them and supplying them. The return value of the function is pushed onto the stack (note: not all natives have return values)
    Jump(isize),       // Jumps ahead or backwards by the specified instruction amount
    JumpFalse(isize), // Jumps ahead or backwards by the specified instruction amount if the top value on the stack is 0
    JumpTrue(isize), // Jumps ahead or backwards by the specified instruction amount if the top value on the stack is not 0
    MakeArray(usize), // Pops the specified number of elements off the stack, reverses them and creates an array containing them
    MakeType(usize), // Creates the custom type at the given index. This will pop the number of arguments needed off the stack, reversing them and supplying them to the custom object, and then pushing the custom object onto the stack
    LoadField(String), // Pops a custom object from the top of the stack and loads the value of its field at the specified index
    LoadString(usize), // Loads a string from the specified index and pushes it onto the stack
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let instr = match self {
            Instruction::PushNumber(n) => format!("PUSHNUM {}", n),
            Instruction::MakeImaginary => "MAKEIMAG".into(),
            Instruction::MakeComplex => "MAKECMPLX".into(),
            Instruction::Add => "ADD".into(),
            Instruction::Subtract => "SUB".into(),
            Instruction::Multiply => "MUL".into(),
            Instruction::Divide => "DIV".into(),
            Instruction::Raise => "RSE".into(),
            Instruction::Remainder => "REM".into(),
            Instruction::Gamma => "GAMMA".into(),
            Instruction::Equals => "EQUALS".into(),
            Instruction::LessThan => "LST".into(),
            Instruction::LessThanOrEquals => "LSTE".into(),
            Instruction::GreaterThan => "GRT".into(),
            Instruction::GreaterThanOrEquals => "GRTE".into(),
            Instruction::Dupe => "DUPE".into(),
            Instruction::Index => "INDEX".into(),
            Instruction::SetLocal(n) => format!("SETLOCAL {}", n),
            Instruction::GetLocal(n) => format!("GETLOCAL {}", n),
            Instruction::SetGlobal(n) => format!("SETGLOBAL {}", n),
            Instruction::GetGlobal(n) => format!("GETGLOBAL {}", n),
            Instruction::PreserveLocal(n) => format!("PRESLOCAL {}", n),
            Instruction::FetchLocal(n) => format!("FETCHLOCAL {}", n),
            Instruction::Call(n) => format!("CALL {}", n),
            Instruction::CallNative(n) => format!("CALLNATIVE {}", n),
            Instruction::Jump(n) => format!("JMP {}", n),
            Instruction::JumpFalse(n) => format!("JMPFALSE {}", n),
            Instruction::JumpTrue(n) => format!("JMPTRUE {}", n),
            Instruction::MakeArray(n) => format!("MAKEARR {}", n),
            Instruction::MakeType(n) => format!("MAKETYPE {}", n),
            Instruction::LoadField(n) => format!("LDFLD \"{}\"", n),
            Instruction::LoadString(n) => format!("LDSTR {}", n)
        };

        write!(f, "{}", instr)
    }
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let instr = match self {
            Instruction::PushNumber(n) => format!("PUSHNUM {}", n),
            Instruction::MakeImaginary => "MAKEIMAG".into(),
            Instruction::MakeComplex => "MAKECMPLX".into(),
            Instruction::Add => "ADD".into(),
            Instruction::Subtract => "SUB".into(),
            Instruction::Multiply => "MUL".into(),
            Instruction::Divide => "DIV".into(),
            Instruction::Raise => "RSE".into(),
            Instruction::Remainder => "REM".into(),
            Instruction::Gamma => "GAMMA".into(),
            Instruction::Equals => "EQUALS".into(),
            Instruction::LessThan => "LST".into(),
            Instruction::LessThanOrEquals => "LSTE".into(),
            Instruction::GreaterThan => "GRT".into(),
            Instruction::GreaterThanOrEquals => "GRTE".into(),
            Instruction::Dupe => "DUPE".into(),
            Instruction::Index => "INDEX".into(),
            Instruction::SetLocal(n) => format!("SETLOCAL {}", n),
            Instruction::GetLocal(n) => format!("GETLOCAL {}", n),
            Instruction::SetGlobal(n) => format!("SETGLOBAL {}", n),
            Instruction::GetGlobal(n) => format!("GETGLOBAL {}", n),
            Instruction::PreserveLocal(n) => format!("PRESLOCAL {}", n),
            Instruction::FetchLocal(n) => format!("FETCHLOCAL {}", n),
            Instruction::Call(n) => format!("CALL {}", n),
            Instruction::CallNative(n) => format!("CALLNATIVE {}", n),
            Instruction::Jump(n) => format!("JMP {}", n),
            Instruction::JumpFalse(n) => format!("JMPFALSE {}", n),
            Instruction::JumpTrue(n) => format!("JMPTRUE {}", n),
            Instruction::MakeArray(n) => format!("MAKEARR {}", n),
            Instruction::MakeType(n) => format!("MAKETYPE {}", n),
            Instruction::LoadField(n) => format!("LDFLD \"{}\"", n),
            Instruction::LoadString(n) => format!("LDSTR {}", n)
        };

        write!(f, "{}", instr)
    }
}
