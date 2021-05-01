use crate::{function::Function, instruction::Instruction, value::Value};
use num_complex::Complex64;
use Instruction::*;
use OptimizationType::*;

enum OptimizationType {
    AddInPlace(f64, f64),
    SubtractInPlace(f64, f64),
    MultiplyInPlace(f64, f64),
    DivideInPlace(f64, f64),
    RaiseInPlace(f64, f64),
    RemainderInPlace(f64, f64),
    GammaInPlace(f64),
    TurnToComplex(f64, f64),
}

pub struct Optimization {
    variant: OptimizationType,
    pos: usize,
}

impl Optimization {
    fn new(variant: OptimizationType, pos: usize) -> Self {
        Optimization { variant, pos }
    }

    pub fn apply_basic_optimization(func: &mut Function) {
        let optimizations = Optimization::find_basic_arithmetic_optimizations(func);

        for optimization in optimizations {
            optimization.apply(func);
        }
    }

    fn find_basic_arithmetic_optimizations(func: &mut Function) -> Vec<Optimization> {
        let instrs = func.instructions();
        let mut optimizations = Vec::new();
        let mut i = 0;

        while i < instrs.len() {
            let instr = &instrs[i];

            match instr {
                Add => {
                    if let PushNumber(n2) = &instrs[i - 1] {
                        if let PushNumber(n) = &instrs[i - 2] {
                            optimizations.push(Optimization::new(AddInPlace(*n, *n2), i));
                        }
                    } else if let MakeImaginary = &instrs[i - 1] {
                        if let PushNumber(n2) = &instrs[i - 2] {
                            if let PushNumber(n) = &instrs[i - 3] {
                                optimizations.push(Optimization::new(TurnToComplex(*n, *n2), i));
                            }
                        }
                    }
                }
                Subtract => {
                    if let PushNumber(n2) = &instrs[i - 1] {
                        if let PushNumber(n) = &instrs[i - 2] {
                            optimizations.push(Optimization::new(SubtractInPlace(*n, *n2), i));
                        }
                    }
                }
                Multiply => {
                    if let PushNumber(n2) = &instrs[i - 1] {
                        if let PushNumber(n) = &instrs[i - 2] {
                            optimizations.push(Optimization::new(MultiplyInPlace(*n, *n2), i));
                        }
                    }
                }
                Divide => {
                    if let PushNumber(n2) = &instrs[i - 1] {
                        if let PushNumber(n) = &instrs[i - 2] {
                            optimizations.push(Optimization::new(DivideInPlace(*n, *n2), i));
                        }
                    }
                }
                Raise => {
                    if let PushNumber(n2) = &instrs[i - 1] {
                        if let PushNumber(n) = &instrs[i - 2] {
                            optimizations.push(Optimization::new(RaiseInPlace(*n, *n2), i));
                        }
                    }
                }
                Remainder => {
                    if let PushNumber(n2) = &instrs[i - 1] {
                        if let PushNumber(n) = &instrs[i - 2] {
                            optimizations.push(Optimization::new(RemainderInPlace(*n, *n2), i));
                        }
                    }
                }
                _ => {}
            }

            i += 1;
        }

        optimizations
    }

    fn apply(&self, func: &mut Function) {
        func.move_cursor_to(self.pos);

        match self.variant {
            AddInPlace(n, n2) => {
                func.remove_instrs(3);
                func.emit_instr(PushNumber(n + n2));
            }
            SubtractInPlace(n, n2) => {
                func.remove_instrs(3);
                func.emit_instr(PushNumber(n - n2));
            }
            MultiplyInPlace(n, n2) => {
                func.remove_instrs(3);
                func.emit_instr(PushNumber(n * n2));
            }
            DivideInPlace(n, n2) => {
                func.remove_instrs(3);
                func.emit_instr(PushNumber(n / n2));
            }
            RaiseInPlace(n, n2) => {
                func.remove_instrs(3);

                let val = if n2.fract() == 0.0 {
                    n.powi(n2 as i32)
                } else {
                    n.powf(n2)
                };

                func.emit_instr(PushNumber(val));
            }
            RemainderInPlace(n, n2) => {
                func.remove_instrs(3);
                func.emit_instr(PushNumber(n % n2));
            }
            GammaInPlace(n) => {
                func.remove_instrs(2);
                let n = Complex64::new(n, 0.0);

                let val = if n.re.fract() == 0.0 {
                    Value::calculate_gamma(n).re as i64 as f64
                } else {
                    Value::calculate_gamma(n).re
                };

                func.emit_instr(PushNumber(val));
            }
            TurnToComplex(re, im) => {
                func.remove_instrs(4);

                func.emit_instr(PushNumber(re));
                func.emit_instr(PushNumber(im));
                func.emit_instr(MakeComplex);
            }
        }

        func.move_cursor_to_end();
    }
}
