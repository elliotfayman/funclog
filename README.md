<div align="center">

# Funclog Documentation

**Contributors**: Elliot Fayman, Ryan Kao, Liliana Rivas  
**Advisor**: Kyle Dewey  
**Date**: May 17th, 2024

</div>

## Table of Contents
1. [Overview](#overview)
2. [Language Justification](#language-justification)
3. [Design](#design)
4. [Basic Examples](#basic-examples)
5. [Modules](#module-overview)
   - [Lexer](#lexer)
   - [Parser](#parser)
   - [Type Checker](#type-checker)
   - [Code Generator](#code-generator)
6. [Usage Guide](#language-interface-guide)
   - [Compilation](#compilation-guide)
   - [Basic Usage](#basic-usage-guide)

## Overview
Funclog is a nondeterministic programming language with functional programming features, designed to support higher-order functions and basic nondeterministic programming. 

## Language Justification
Funclog stands out by offering higher-order functions in a nondeterministic context, a feature rarely found in commercial logic programming languages.

## Design
The Funclog compiler generates Prolog code using Scala, leveraging its functional programming capabilities. The compiler is modular, consisting of the Lexer, Parser, Type Checker, and Code Generator, ensuring clarity and maintainability.

## Basic Examples
Here are some key features of Funclog:

**Nondeterminism**
```
(def foo () int (choice (return 1) (return 2)))
```
Creates nondeterministic behavior within software.


**Assertions**
```
(def bar (int x) int (block (assert (< x 5)) (return x)))
```
Applies assertions within software.


**Higher-Order Functions**
```
(def bash ((=> ( ) int) func) int (return (call func ())))
```
Allows passing and calling functions.

### Example Conversion
**Funclog:**
```
(def f (int X) int (choice 
 (block 
   (var (=> (int) int) Z (=>  (int Inner) (return Inner)))            
   (var (=> ((=> (int) int)) int) Y (=> ((=> (int) int) Func) (return (call Func (5))))) 
   (return (call Y (Z)))
 ) 
 (return X))
)
```

**Prolog:**

```
lambda_func4(XInner, Rtn4) :- 
  Rtn4 is XInner.

lambda_func12(Rtn12) :- 
  lambda_func4(5, R0), 
  Rtn12 is R0.

func_f(XX, RtnV21) :- 
  (true, 
   true, 
   lambda_func12(Rtn12), 
   RtnV21 is Rtn12, 
   ; 
   RtnV21 is XX).
```

## Module Overview
The Funclog compiler consists of the Lexer, Parser, Type Checker, and Code Generator. The compilation process involves:

1. Tokenizing the source file using the Lexer.
1. Generating an abstract syntax tree (AST) with the Parser.
1. Checking the AST for type correctness with the Type Checker.
1. Emitting semantically equivalent Prolog code with the Code Generator.

### Lexer
Processes strings into tokens. Tokens include Basic Type Tokens, Identifier Tokens, Operator/Grammar Tokens, and Reserved Word Tokens.

### Parser
Creates an abstract syntax tree (AST) from tokens, using Scala parser combinator library for enhanced simplicity and readability.

### Type Checker
Enforces static typing rules, catching type-related errors early and improving program stability.

### Code Generator
Transforms a well-typed AST into executable Prolog code, employing defunctionalization techniques for higher-order functions.

## Language Interface Guide

### Compilation Guide
Dependencies: Git, Scala, Scala Build Tool (SBT). Clone the repository and compile the compiler:
```bash
git clone 'https://github.com/elliotfayman/funclog'
sbt compile
```

### Basic Usage Guide
Create a .funclog program and compile it into Prolog:
```bash
sbt "run path/to/yourfile.funclog"
```
Use the compiled output in a Prolog engine.




