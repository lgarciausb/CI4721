# Language Definition

## Language Summary 

- Structural Statically Typed 
- Atoms for nominal typing.
- Vector oriented, operations are lifted to vectors.
- Strict evaluation from left to right.
- Lazy `||` (or) and `&&` (and) expressions.
- With pattern matching.

## Abstract syntax

### General

```
id -> [a-zA-Z_][a-zA-Z_0-9]*
lvaluable -> id [\[e\]]*
lvaluable -> id.lvaluable
args -> e [,args]*
fun_args -> T id [,fun_args]*
loop_a -> break;
loop_a -> continue;
loop_a -> a;
patterns -> n where 'n' is a number
patterns -> 'c' where 'c' is a character 
patterns -> "s" where 's' is a sequence of characters 
patterns -> id [by reference]^1 
patterns -> {id : T [,records]*} [by reference]^1
records -> id : T [,records]*
function_return -> bool, char, string, int, float, unit, void
```

### Types

```
T -> bool, char, string, int, float, unit, void, #any_atom
T -> {id : T [,records]*}
T -> T | T
```

### Expressions 

```
e -> new T(args*)
e -> &lvaluable
e -> id(args*)
e -> e + e
e -> e * e
e -> e / e
e -> e ^ e
e -> e % e
e -> e - e
e -> -:call v:lua.require("markdown").new_line_below()e
e -> e < e
e -> e > e
e -> e != e
e -> e == e
e -> e >= e
e -> e <= e
e -> e || e
e -> e && e
e -> ~e
e -> (e)
e -> \[args*\]
e -> n where 'n' is any number
e -> c where 'c' is a character
e -> "s" where 's' is any sequence of letters
e -> match e with [patterns => {a}]+
```

### Actions 

```
a -> lvaluable = e;
a -> for(id : id){loop_a*}
a -> while(e){loop_a*}
a -> match e with [patterns => {a}]+
a -> e;
a -> return e;
```

### Definitions 

```
function_return id(fun_args*){a}
```

## Types

### Basics

Basic types and their inhabitants constitutes:

- Booleans: $true,false \in bool$
- Characters: $'a' \in char$
- Strings: $"a" \in string$
- Integer: $5 \in int$
- Floating point numbers: $5.5,5 \in float$
- unit: $unit \in unit$
- void: inhabited type.
- vector: $\[1,2,3\] \in vector<int>$
- atoms: $\#atom \in \#atom$
- Pointers: $\&x \in pointer<int>$

Note1: Number constants are "polymorphic". That is $5$ can be an integer
or a float depending on the context.

Note2: atoms (aka: brands) are unique identifiers used to provide nominal typing.



### Object types 

The language provides structural static typing with the following syntax:

- For records: `{foo : T, bar : T}`
- For tagged unions: `T | T`



## Actions

### Comments

Multiline comments are provided via: `/**/`

```c
/* this is a comment*/
/* 
this 
is 
also
a comment */
```

### Assignments

Assignments can be done for every syntactic construct that posses and l-value. And will always
have the following syntax:

```c
T lvaluable = e;
```


### Pattern Matching

Expressions can be pattern match at the action level:

```c
bool x = true;
match x with 
  true =>  {...}
  false => {...}
```

It is important that the two branches of code must have the same return type. 
Aka: the last instruction of each block must have the same type.

Pattern matching can also bind new names. Which will always be treated as pass by value unless
explicitely stated otherwise:

```c
type wrapped = {w : vector<int>}
:
:
:
wrapped ws = new wrapped([1,2,3]);
match ws with 
  {w : vector<int>} => {append(w,1);}
/* ws.w will still be [1,2,3] */

match ws with 
  {w : vector<int>} by reference => {append(w,1);}
/* ws.w will bw [1,2,3,1] */
```

For the time being. Patterns must be exact. That is, no record subtyping nor row polymorphism.


### Iteration

The language provides `for-each` loops (only for vector types) and `while` loops (only for bool types):

```c
for(var : vector){...}
while(bool){...};
```

A concrete example:

```c
vector<string> xs = ["1","2","3"];
for(x : xs){
  print(x);
}
while(true){
  print("forever")
}
```

The bindings of a `for-each` are taken by value for the time being.

It also provides `break`s and `continue`s inside any iteration mechanism:

```c
for(x : xs){
  match x with:
    0 => {break;}
    1 => {continue;}
    _ => {...}
  
}
```

## Expresiones


Aside the well known arithmetic, boolean and function application expressions, we also provide the following syntax for
constructors:

```c
new T(args)
type wrapped = {w : vector<int>}
new  wrapped([1,2,3]);
```

Additionally, binary operations are also lifted pointwise for vectors, that is, we automatically
apply the Zip Applicative instance for every vector.


Notice that match constructs can also be used anywhere a expression can.

## Functions 

Functions are defined almost C-style:

```c
function_return id(fun_args*){a}

int my_fun(int a, wrapped w by reference){...}
```

The entry-point of every program is the `main` function.

## Standard library

We currently provide the following standard lirary:

```haskell
input :: string -> string 
print :: string -> unit 
to_string :: int | double | bool | char -> string 
to_int :: string | char -> int 
to_double :: string | char -> double 
append<a> :: vector<a> -> a -> unit 
pop :: vector<a> -> unit
(&) :: lvaluable -> Pointer<lvaluable>
```

## ALMOST EVERYTHING (SYNTAX, STANDARD FUNCTIONS,...) MINUS THE FEATURES IS SUBJECT TO CHANGE
