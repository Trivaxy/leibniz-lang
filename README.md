# Leibniz
Leibniz is an interpreted but experimental programming language revolving around mathematics.
Writing Leibniz should feel natural, and the language itself is very simple. Currently, the parser and runtime are only ~1k LOC.

# Quick tutorial
Leibniz currently has three data types, until the rest are implemented:
- `Number`: The most basic data type. It's a complex number with double-precision real and imaginary components. Leibniz makes the distinction between real and complex numbers depending on whether or not there is an imaginary component.
- `Vector`: As the name implies, it's a vector, which is a pair of two `Number`s that must be real.
- `Array`: A collection of different values. Arrays are not limited to storing only one data type.

Almost everything in Leibniz is an expression (the exception being declarations of functions and variables).

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

Imaginary numbers are built into the language syntax as the symbol `i`.
```rust
5+90i / 2i^i // 1383.1689451587715+1665.182851006511i
sin(pi * i) // 11.548739357257746i
let imaginaryi = -1^0.5 // i
```

Conditionals are supported, too. This brings us to the next point: Leibniz has no concept of true / false booleans like other languages. Much like C, it considers any non-zero number to be truthy, while zero is considered false-y. This means that the conditional operators Leibniz has will return `1` or `0` when used.
```rust
let x = 5 < 9 // 1
let y = x > x + 2 // 0
```

This can be used with Leibniz's conditional operator, `=>`, which expects a predicate (any `Number` with only a real component), a true arm if the predicate is not zero, and a false arm if the predicate is zero. The syntax is like so:
```rust
predicate_expression => true_expression | false_expression
```
And it can be used, for example, like this:
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

Leibniz has another construct, called ranges, which can act as a looping mechanism.
```rust
x: [0..10, 1] => x * 3
```

This is similar to for-loops in other languages. Leibniz will step through from `0` to `10`, both bounds inclusive, with a step of `1`, and assign it to the temporary variable `x`. The body will then evaluate for each `x`, and you can do whatever you want with it. The bounds and step can also be decimals instead of integers. Range syntax is like so:
```rust
variablename: [firstbound..secondbound, step] => expression
```

Another powerful feature of ranges is that they are also an expression, which implicitly evaluates to their *sum*. Thus, the following snippet of code:
```rust
x: [0..10, 1] => x * 3
```
actually evaluates to `165`. (`0 + 3 + 6 + 9 + 12 + 15 + 18 + 21 + 24 + 27 + 30`)

This makes ranges almost the equivalent of the sigma notation in math, the primary difference being you can also loop from an upper bound to a lower bound, and ranges don't strictly have to be integers:
```rust
z: [20..0, 0.5] => z^2 // 5535 
```

Note: ranges will not go outside their bounds, even if the step allows it. For example:
```rust
p: [0..10, 3] => p
```
evaluates to `28`. This is because rather than overstepping the over `10` bound (and in turn evaluating `0 + 3 + 6 + 9 + 12`) it will short circuit the last step into the upper bound, so `0 + 3 + 6 + 9 + 10` is evaluated instead. This implementation detail is subject to change in the future.

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

Leibniz's third data type is the `Array`. Their syntax is extremely similar to other languages.
```rust
[3, 9, 10, 5] // An array containing 4 real numbers
```

Arrays are not limited to just one data type:
```rust
[3, 10.52, 5i, (9 + 20i)^0.5, vec(20, 9.5)]
[[0, 0.5, 0.744], [9, 20i, [0, 0], 4]] // An array containing arrays
```

You can access each element in an array by indexing it, which is identical to other languages.
Array indices are zero-based, meaning that the first element has index 0.
```rust
let x = [5, 9i, 4]
let y = x[0] // 5
let z = y + x[1] // 5 + 9i
```

You can add an element to the end of an array by adding other values to it:
```rust
let x = [0, 20, 5.2]
x = x + 5 // [0, 20, 5.2, 5]
```

There are a handful of functions in the standard library that make using arrays easy.

The `len(x)` function, where `x` is an array, will return the number of elements in `x`.
```rust
let x = [50, [20, 4], 0, 5.1]
len(x) // 4
len(x[1]) // 2
```

You can remove elements from arrays using `rm(x, y)` where `x` is an array and `y` is an integer greater than or equal to 0.
`rm(x, y)` does not modify the array in-place, it returns a copy of the array with the element at index `y` removed.
```rust
let x = [0, 20, 10]
rm(x, 1) // [0, 10]
x // [0, 20, 10]
```

If you need to add an element to an array at a particular position (where adding to the array won't suffice), you can use the `ins(x, y, z)` function, where `x` is an array, `y` is an integer and `z` is any value.
`ins(x, y, z)` will push the value `z` into the array `x` at position `y`. Like `rm(x, y)`, this will create a copy of the array with the new value pushed.
```rust
let x = [2, 4, 12, 20]
ins(x, 2, i) // [2, 4, i, 12, 20]
```

Looping through arrays is quite common. Leibniz ranges are well suited for doing so.
```rust
let x = [5, 10, 90, 20, 5.5, 2.3, 5.66767, -6, -45]

z: [0..len(x) - 1, 1] => x[z] // 87.46767
```
Remember that ranges evaluate to the sum of everything their body evaluates to? Using `x[z]`, the range ends up summing everything in the array!

What if we want to sum every even number in an array? Bring in a conditional:
```rust
let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

z: [0..len(x) - 1, 1] => (x[z] % 2) == 0 => x[z] | 0  // 30
```


# Todo
- Implement the rest of Leibniz types, such as matrices, imaginary numbers and so on
- Make the interpreter optimize Leibniz code
- Work on the Leibniz standard library
- Make it possible to create graphical interactions using Leibniz
- Explore the possibility of automatic derivation and integration


