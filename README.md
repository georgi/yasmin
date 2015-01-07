Yasmin
======

A simple statically typed functional language. 

## Install

* `brew install llvm`
* `opam install llvm`
* `make`

## Run

Currently there is only a simple REPL:

`./yasmin.byte`

You need to terminate each statemnent or definition with a semicolon.

## Types

* bool
* byte
* int16
* int32
* int
* float
* void
* string (aka byte[])
* arrays (int[], byte[])
* function

## Functions

Last statement is returned.

```
def add(int x)
  x + 10;
  x + x 
end

add(10)
```

## Variables

Cannot be reassigned.

```
let x = "Hello" in
  let y = " World" in
    puts(x + y)
  end
end
```

## Conditional

Both clauses must be defined and have same type.

```
if x < y then
  x + y
else
  x - y
end
```

## Anonymous functions

Lexical scoped functions, in this example the closure captures the state of
the function parameter `y`:

```
def add(int y)
  fun(int x) { x + y }
end

let f = add(10) in
  f(10)
end
```

## Arrays and strings

Arrays and strings are modelled as structs holding the lenght information.

```
let x = [1,2] in 
  x[0] + x[1] 
end

let y = "Hello" in 
  x[0] + x[1]
end
```

## Structs

```
struct MyStruct
  int x;
  int y;
end
```

## Anonymous structs

```
let x = { a: 10, b: 20 } in
  x.a + x.b
end
```
