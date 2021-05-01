use core::panic;
use std::{fmt, ops};

use linked_hash_map::LinkedHashMap;
use num_complex::Complex64;

#[derive(Clone, Debug)]
pub enum Value {
    Real(f64),
    Complex(Complex64),
    Array(Vec<Value>),
    Custom(LinkedHashMap<String, Value>),
}

use Value::*;

impl Value {
    pub fn real(n: f64) -> Self {
        Real(n)
    }

    pub fn as_f64(self) -> f64 {
        match self {
            Real(n) => n,
            _ => panic!("failure to convert to f64. this should not happen"),
        }
    }

    pub fn as_complex64(self) -> Complex64 {
        match self {
            Complex(c) => c,
            Real(n) => Complex64::new(n, 0.0),
            _ => panic!("failure to convert to Complex64. this should not happen"),
        }
    }

    pub fn as_custom(self) -> LinkedHashMap<String, Value> {
        match self {
            Custom(fields) => fields,
            _ => panic!("failure to convert to Custom. this should not happen"),
        }
    }

    pub fn imaginary(n: f64) -> Self {
        Complex(Complex64::new(0.0, n))
    }

    pub fn complex(r: f64, i: f64) -> Self {
        Complex(Complex64::new(r, i))
    }

    pub fn is_custom(&self) -> bool {
        match self {
            Custom(_) => true,
            _ => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Real(n) => match other {
                Real(n2) => n == n2,
                Complex(c) => c.im == 0.0 && *n == c.re,
                _ => false,
            },
            Complex(c) => match other {
                Real(n) => c.im == 0.0 && c.re == *n,
                Complex(c2) => c == c2,
                _ => false,
            },
            Array(arr) => match other {
                Array(arr2) => {
                    if arr.len() != arr2.len() {
                        return false;
                    }

                    for i in 0..arr.len() {
                        if arr[i] != arr2[i] {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },
            Custom(values) => match other {
                Custom(other_values) => {
                    for key in values.keys() {
                        if !other_values.contains_key(key) {
                            return false;
                        } else if values[key] != other_values[key] {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },
        }
    }
}

impl Eq for Value {}

impl ops::Add<Value> for Value {
    type Output = Value;

    fn add(mut self, mut rhs: Value) -> Self::Output {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n + n2),
                Complex(c) => Complex(n + c),
                Array(ref mut vals) => {
                    vals.push(self);
                    rhs
                }
                Custom(_) => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c + n),
                Complex(c2) => Complex(c + c2),
                Array(ref mut vals) => {
                    vals.push(self);
                    rhs
                }
                Custom(_) => Value::error(),
            },
            Array(ref mut vals) => {
                vals.push(rhs);
                self
            }
            Custom(_) => Value::error(),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> Self::Output {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n - n2),
                Complex(c) => Complex(n - c),
                _ => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c - n),
                Complex(c2) => Complex(c - c2),
                _ => Value::error(),
            },
            _ => Value::error(),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = Value;

    fn mul(self, rhs: Value) -> Self::Output {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n * n2),
                Complex(c) => Complex(n * c),
                _ => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c * n),
                Complex(c2) => Complex(c * c2),
                _ => Value::error(),
            },
            _ => Value::error(),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = Value;

    fn div(self, rhs: Value) -> Self::Output {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n / n2),
                Complex(c) => Complex(n / c),
                _ => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c / n),
                Complex(c2) => Complex(c / c2),
                _ => Value::error(),
            },
            _ => Value::error(),
        }
    }
}

impl ops::Rem<Value> for Value {
    type Output = Value;

    fn rem(self, rhs: Value) -> Self::Output {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n % n2),
                Complex(c) => Complex(n % c),
                _ => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c % n),
                Complex(c2) => Complex(c % c2),
                _ => Value::error(),
            },
            _ => Value::error(),
        }
    }
}

impl Value {
    pub fn pow(self, rhs: Value) -> Self {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n.powf(n2)),
                Complex(c) => Complex(self.as_complex64().powc(c)),
                _ => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c.powf(n)),
                Complex(c2) => Complex(c.powc(c2)),
                _ => Value::error(),
            },
            _ => Value::error(),
        }
    }

    pub fn rem(self, rhs: Value) -> Self {
        match self {
            Real(n) => match rhs {
                Real(n2) => Real(n % n2),
                Complex(c) => Complex(n % c),
                _ => Value::error(),
            },
            Complex(c) => match rhs {
                Real(n) => Complex(c % n),
                Complex(c2) => Complex(c % c2),
                _ => Value::error(),
            },
            _ => Value::error(),
        }
    }

    pub fn error() -> Self {
        Value::Custom(LinkedHashMap::new())
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Custom(n) => n.is_empty(),
            _ => false,
        }
    }

    pub fn calculate_gamma(c: Complex64) -> Complex64 {
        const P: [f64; 8] = [
            676.5203681218851,
            -1259.1392167224028,
            771.32342877765313f64,
            -176.61502916214059,
            12.507343278686905,
            -0.13857109526572012,
            9.9843695780195716e-6,
            1.5056327351493116e-7,
        ];

        const PI: Complex64 = Complex64::new(std::f64::consts::PI, 0.0);

        let mut c = c;

        if c.re < 0.5 {
            Self::calculate_gamma(PI / (((PI * c).sin()) * (1.0 - c)))
        } else {
            let mut x = Complex64::new(0.99999999999980993, 0.0);
            c -= 1.0;

            for i in 0..P.len() {
                x += P[i] / (c + (i as f64) + 1.0)
            }

            let t = c + (P.len() as f64) - 0.5;
            (2.0 * PI).sqrt() * t.powc(c + 0.5) * (-t).exp() * x
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Real(n) => write!(f, "{}", n),
            Complex(c) => {
                if c.im == 0.0 {
                    write!(f, "{}", c.re)
                } else if c.re == 0.0 {
                    if c.im == 1.0 {
                        write!(f, "i")
                    } else {
                        write!(f, "{}i", c.im)
                    }
                } else {
                    if c.im == 1.0 {
                        write!(f, "{} + i", c.re)
                    } else if c.im == -1.0 {
                        write!(f, "{} - i", c.re)
                    } else if c.im > 0.0 {
                        write!(f, "{} + {}i", c.re, c.im)
                    } else {
                        write!(f, "{} - {}i", c.re, c.im * -1.0)
                    }
                }
            }
            Array(arr) => {
                let elements = arr
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>();

                write!(f, "[{}]", elements.join(", "))
            }
            Custom(fields) => {
                let fields = fields
                    .iter()
                    .map(|kvp| format!("{}: {}", kvp.0, kvp.1))
                    .collect::<Vec<String>>();

                write!(f, "<{}>", fields.join(", "))
            }
        }
    }
}
