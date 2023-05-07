
# Forth Interpreter in Haskell

Implements a very simple subset of Forth

Inspired by https://exercism.org/tracks/haskell/exercises/forth

## Run
```sh
ghci forth.hs
```

## Usage 

Calculate 2 + 8
```haskell
*Forth> run "2 8 +"
Right ForthState [10]
```

Push 1 to stack and duplicate it three times
```haskell
*Forth> run "1 DUP DUP DUP"
Right ForthState [1,1,1,1]
```

Calculate 5 + (5\*8)
```haskell
*Forth> run "5 8 OVER * +"
Right ForthState [45]
```

## Instructions

| Instruction |                                 | Description                 |
|:-----------:|---------------------------------|:----------------------------|
|      +      | [a, b, ...] -> [a+b, ...]     | Pop two items, a and b, from stack. Push a+b to top  |
|      -      | [a, b, ...] -> [a-b, ...]     | Pop two items, a and b, from stack. Push a-b to top  |
|     \*      | [a, b, ...] -> [a\*b, ...]    | Pop two items, a and b, from stack. Push a\*b to top |
|      /      | [a, b, ...] -> [a/b, ...]     |  Pop two items, a and b, from stack. Push a/b to the top<br /> Returns Left DivisionByZero if b is 0 |
|    DUP      | [a, b, ...] -> [a, a, b, ...] | Copy the top stack item and push it to the top       |
|    DROP     | [a, b, ...] -> [b, ...]       | Discard the top of the stack                         |
|    SWAP     | [a, b, ...] -> [b, a, ...]    | Pop two values, a and b, from the stack.<br />Push them back in swapped order |
|    OVER     | [a, b, ...] -> [b, a, b, ...] | Copy the next value after the top of the<br /> stack and push it to the top    |
| Any number  | [a, b, ...] -> [x, a, b, ...] | Push number to top of stack. Integers only |


## Read more

#### Forth (programming language), Wikipedia

https://en.wikipedia.org/wiki/Forth_(programming_language)#Overview

#### Starting _FORTH_

https://www.forth.com/starting-forth/0-starting-forth/
