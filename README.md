# Leibniz
Leibniz is an interpreted but experimental programming language revolving around mathematics.
Writing leibniz should feel natural, and the language itself is very simple. Currently, the parser and runtime are only ~1k LOC.

# Quick tutorial
Leibniz currently has two data types, until the rest are implemented:
- `RealNumber`: The most basic data type. It's a double precision floating point number, ie just a decimal.
- `Vector`: As the name implies, it's a vector, which is a pair of two `RealNumber`s.

Almost everything in leibniz is an expression (the exception being declarations of functions and variables).

An expression can come in many different ways. For example
```rust
5
```
is a completely valid expression which evaluates to `5`.

Leibniz operators follow natural operator precedence.
```rust
5 + 9 * 2^3 // 77
(5 + 9) * 2^3 // 112
```
(`^` is the power operator, implemented as part of the language rather than as a function)

You can create variables.
```rust
let x = 5
let y = x^2
let z = x + y * 2
z // 60
```

Leibniz makes declaring functions extremely easy:
```rust
let f(x) = x * 2

let x = f(9) // 18

let g(x, y, z) = x^y^z + f(x)
```

Conditionals are supported, too. This brings us to the next point: Leibniz has no concept of true / false booleans like other languages. Much like C, it considers any non-zero number to be truthy, while zero is considered false-y. This means that the conditional operators Leibniz has will return `1` or `0` when used.
```rust
let x = 5 < 9 // 1
let y = x > x + 2 // 0
```

This can be used with Leibniz's conditional operator, `=>`, which expects a predicate (any `RealNumber`), a true arm if the predicate is not zero, and a false arm if the predicate is zero.

```rust
let x = 5
let y = x > 2 => 20 | x + 5 // 20
x = 1 // leibniz allows you to reassign variables within the current scope
y = x > 2 => 20 | x + 5 // 6
```

From this you can see that `=>` will execute the first arm (true) if the predicate is not `0`, otherwise it will execute the second arm (false). For example, a conditional that is always true can be written as:
```rust
1 => 6 | 9 // 6
```

Recursion is also supported in Leibniz. Here's a factorial function:
```rust
let fact(x) = x < 1 => 1 | x * fact(x - 1)
```

Leibniz supports a construct called *trees*, which lets you execute and evaluate many things in succession, but only the last expression is returned as the value of the tree.
```rust
let z = 5
let x = {
    let y = 90 + 2
    //
    // other awesome code here...
    // 
    z * y // this is what the tree returns and becomes the value of x
}
```

You can have trees inside trees.
```rust
let x = {
    {
        {
            // ...
        }
    }
}
```

Keep in mind that trees *must* end with an expression, otherwise you will get an error. This means that
```rust
let x = {
    let y = 9
}
```
is invalid, because `let y = 9` is not an expression in Leibniz. However:
```rust
let x = {
    let y = 9
    y
}
```
is valid, because `y` alone is a valid expression.

Leibniz has another construct, called ranges, which can act as a looping construct.
```rust
x: [0, 10, 1] => x * 3
```

This is similar to for-loops in other languages. Leibniz will step through from `0` to `10`, both bounds inclusive, with a step of `1`, and assign it to the temporary variable `x`. The body will then evaluate for each `x`, and you can do whatever you want with it. The bounds and step can also be decimals instead of integers.

Another powerful feature of ranges is that they are also an expression, which implicitly evaluates to their *sum*. Thus, the following snippet of code:
```rust
x: [0, 10, 1] => x * 3
```
actually evaluates to `165`. (`0 + 3 + 6 + 9 + 12 + 15 + 18 + 21 + 24 + 27 + 30`)

This makes ranges almost the equivalent of the sigma notation in math, the primary difference being you can also loop from an upper bound to a lower bound, and ranges don't strictly have to be integers.

Let's move on to Leibniz's second data type, `Vector`.

Creating a vector is possible through the builtin `vec` function Leibniz provides. It takes an `x` and `y` component for the vector.
```rust
let myvec = vec(2, 5)
```

`Vector`s can be multiplied, divided and raised to the power of `RealNumber`s. Other operations regarding them are not valid, thus the standard library (heavy WIP) offers functions that help with manipulating and working with vectors.

You can access the x and y component of a `Vector` through the builtin `x` and `y` functions. Each return the respective component of the `Vector`.
```rust
let myvec = vec(2, 5) // (2, 5)
x(myvec) // 2
y(myvec) // 5
```

# Todo
- Implement the rest of Leibniz types, such as matrices, imaginary numbers and so on
- Make the interpreter optimize Leibniz code
- Work on the Leibniz standard library
- Make it possible to create graphical interactions using Leibniz
- Explore the possibility of automatic derivation and integration


