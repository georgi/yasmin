Yasmin
======

A simple c-like language for my own pleasure.

## Types

* bool
* byte
* int16
* int32
* int (defaults to int64)
* float
* void
* pointer (like `byte*`)
* function

## Functions

C-style types and function declaration.

```
int add(int x) {
  x + x;
}

add(10);
```

## Variables

Local variable types are inferred and can be assigned only once.

```
x = "Hello";
y = " World";
puts(x + y);
```
