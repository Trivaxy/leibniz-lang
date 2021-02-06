# Leibniz
Leibniz is an interpreted but experimental programming language revolving around mathematics.
Writing Leibniz should feel natural, and the language itself is very simple. Currently, the parser and runtime are only ~1.5k LOC.

# Quick tutorial
Got 10 minutes to spare? This'll make you master the language. Yes, it's that simple.

Leibniz is fundamentally about mathematics, and it tries to stay as close to normal mathematical notation as possible (for a programming language, that is).

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

Leibniz actually has the factorial operator built into the language in `!` notation.
```rust
0! // 1
5! // 120
3!! // 720
```
Be aware: the factorial operator takes precedence over all other operators.
```rust
5 + 3! // 11
(5 + 3)! // 40320
3^2! // 9
(3^2)! // 362880
```

Leibniz allows you to create variables.
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

Imaginary numbers are built into the language syntax as the symbol `i`, and can be added to real numbers to form complex numbers. You can do lots of operations with them.
```rust
(5+90i) / (2i)^i // 295.1358207096044 + 317.66802636620474i
sin(pi * i) // 11.548739357257746i
let awesomenumber = -1^0.5 // i
```

Remember the factorials from earlier? For numbers that are not positive real integers (or `0`), `!` will use the gamma function to calculate a result.
```rust
i! // 0.4980156681183563 - 0.15494982830181042i
2.5! // 3.3233509704478426
(10-3.5i)! // -871441.4094531853 - 1852101.839324699i
```

Of course, with the existence of complex numbers, Leibniz has two functions you may be familiar with.
- `Re(x)` where `x` is any number. It'll take out the real component of `x` and return it
- `Im(x)` where `x` is any number. It'll take out the imaginary component of `x` and return it
```rust
Re(5+3i) // 5
Im(2-9i) // -9
```

Leibniz supports conditionals. This brings us to the next point: Leibniz has no concept of true / false booleans like other languages. Much like C, it considers any non-zero number to be truthy, while zero is considered false-y. This means that the conditional operators Leibniz has will return `1` or `0` when used.
```rust
let x = 5 < 9 // 1
let y = x > x + 2 // 0
```

This can be used with Leibniz's conditional operator, `=>`, which expects a predicate (any real number), a true arm if the predicate is not zero, and a false arm if the predicate is zero. The syntax is like so:
```rust
predicate_expression => true_expression | false_expression
```
And it can be used, for example, like this:
```rust
let x = 5
let y = x > 2 => 20 | x + 5 // 20
x = 1 // note: leibniz allows you to reassign variables within the current scope
y = x > 2 => 20 | x + 5 // 6
```

From this you can see that `=>` will execute the first arm (true) if the predicate is not `0`, otherwise it will execute the second arm (false). For example, a conditional that is always true can be written as:
```rust
1 => 6 | 9 // 6
```

Recursion is also supported in Leibniz. Let's forget Leibniz has factorial syntax and make a function ourselves:
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

Leibniz's second data type is the `Array`. Their syntax is extremely similar to other languages.
```rust
[3, 9, 10, 5] // An array containing 4 real numbers
```

Arrays are not limited to just one data type:
```rust
[3, 10.52, 5i, (9 + 20i)^0.5, vec2(20, 9.5)]
[[0, 0.5, 0.744], [9, 20i, [0, 0], 4]] // An array containing arrays
```

You can access each element in an array by indexing it, which is identical to other languages.
Array indices are zero-based, meaning that the first element has index 0.
```rust
let x = [5, 9i, 4]
let y = x[0] // 5
let z = y + x[1] // 5 + 9i
```

You can append an element to the end of an array by adding a value to it:
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

Leibniz allows you to define your own custom value types. Let's say, for example, that we want to define a circle.
A circle has a radius, so we'll start from there.
```rust
let circle = <radius~real>
```

This defines a circle as a value with a `radius` property, which must be a `real` number. We can create a circle like so:
```rust
let mycircle = circle(5) // this will make us a circle with radius 5
```

We can get the radius of our circle like this:
```rust
radius(mycircle) // 5
```

It'd be nice if we gave our circle a position as well on the XY plane. Let's update our definition of a circle:
```rust
let circle = <radius~real, x~real, y~real>
```
Now our circle has three properties: `radius`, `x` and `y`, all of which must be real numbers.

Let's recreate our circle variable.
```rust
let mycircle = circle(5, 10, -2.5)
```

Like before, we can get the properties of our circle like this:
```rust
radius(mycircle) // 5
x(mycircle) // 10
y(mycircle) // -2.5
```

Great, we have a circle, but let's give it some practical purpose. How about an area and circumference? Those should be functions.
```rust
let area(c~circle) = pi * radius(c)^2

let circumf(c~circle) = 2 * pi * radius(c)
```

You can see some new syntax in our functions here. `c~circle` indicates that the parameter `c` must be a `circle`. If this type bound is omitted, then by default the bound is `real`, which means we can rewrite our circle definition like so:
```rust
let circle = <radius, x, y>
```

Did you know Leibniz's standard library has a custom value type `vec2`? Its definition is extremely simple.
```rust
let vec2 = <x, y>
```

Let's put that into our circle definition, and recreate our circle variable.
```rust
let circle = <radius, pos~vec2>

let mycircle = circle(5, vec2(10, -2.5))
print(mycircle) // circle <radius: 5, pos: vec2 <x: 10, y: 10>>
```

Creating type definitions lets you group properties together to represent concepts very nicely.
The built-in type bounds are:
- `real`: Any real number
- `imaginary`: Any imaginary number
- `complex`: Any number
- `integer`: Any real integer number
- `natural`: Any real natural number
- `whole`: Any whole number
- `array`: Any array
- `any`: Any value

There is another thing about Leibniz that you should know, and it's that values do not have a unique identity. This means that things with the same value will truly be equal.
```rust
5 == 5 // 1
5 == 5.0 // 1
(5 - 9i) == (-9i + 5) // 1
[1, 2, 3, 4] == [1, 2, 3, 4] // 1
[1, 2, 3, 4] == [4, 3, 2, 1] // 0
[] == [] // 1
vec2(10, 5) == vec2(10, 5) // 1
vec2(9, 3) == vec2(2, -7) // 0
circle(5, vec2(2, 3)) == circle(5, vec2(2, 3)) // 1
```

# Miscellaneous functions
- `mem(x)` where `x` is any value. Returns the memory usage of the value in bytes
- `clock(x)` where `x` is any real number. Returns the total time elapsed in seconds since the Leibniz script began executing, subtracted by `x`

# Todo
- Make the interpreter optimize Leibniz code
- Work on the Leibniz standard library
- Make it possible to create graphical interactions using Leibniz
- Explore the possibility of automatic derivation and integration


