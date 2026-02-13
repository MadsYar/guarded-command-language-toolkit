# Guarded Command Language Compiler, Intepreter and Program Analysis

It was developed as part of the 02141 Computer Science Modelling course at the Technical University of Denmark with Jacopo Ceccuti. This project implements a complete toolchain from lexical analysis through program verification and security analysis.

![Structure of the Project](overview.png)

## Table of Contents

- [Overview](#overview)
- [Language Support](#language-support)
- [Features](#features)
- [Architecture](#architecture)
- [Components](#components)
- [Technology Stack](#technology-stack)
- [Usage](#usage)
- [Project Structure](#project-structure)

## Overview

This project implements a full-featured development environment for the Guarded Command Language (GCL), a non-deterministic programming language featuring guarded commands. The toolkit demonstrates core compiler construction techniques and program analysis methods used in modern programming language implementations.

**Key Achievement:** Built a complete language implementation from scratch, including:
- Lexical analysis and parsing with FsLex/FsYacc
- AST construction and transformation
- Program graph generation for both deterministic and non-deterministic execution
- Concrete semantics interpretation
- Static program analysis (sign analysis)
- Formal program verification
- Information flow security analysis

## Language Support

The Guarded Command Language (GCL) includes:

**Arithmetic Expressions:**
- Basic operators: `+`, `-`, `*`, `/`, `^` (exponentiation)
- Unary minus
- Variables and array access: `x`, `A[i]`

**Boolean Expressions:**
- Logical operators: `&`, `|`, `&&`, `||`, `!`
- Comparison operators: `=`, `!=`, `>`, `>=`, `<`, `<=`

**Commands:**
- Assignment: `x := expr`
- Array assignment: `A[i] := expr`
- Skip statement: `skip`
- Sequential composition: `C1 ; C2`
- Conditional: `if b1 -> C1 [] b2 -> C2 fi`
- Loops: `do b1 -> C1 [] b2 -> C2 od`

## Features

### 1. **Parser & Pretty Printer**
- Lexical analysis using FsLex with comprehensive token recognition
- Syntax analysis using FsYacc with proper precedence and associativity
- Abstract Syntax Tree (AST) generation
- Pretty printing for code formatting and AST visualization

### 2. **Program Graph Compiler**
- Transforms AST into executable program graphs
- Support for both **deterministic** and **non-deterministic** execution models
- Generates DOT format output for graph visualization
- Handles complex control flow including nested conditionals and loops

### 3. **Interpreter**
- Execute GCL programs with concrete semantics
- Memory management for variables and arrays
- Tracks complete execution sequences
- Termination state detection (Running, Stuck, Terminated)
- Step-by-step execution tracing

### 4. **Program Verification**
- Generates verification conditions using predicate logic
- Implements strongest postcondition (`sp`) calculus
- Handles loop invariants and preconditions/postconditions
- Substitution and quantifier manipulation
- Support for first-order logic predicates

### 5. **Sign Analysis**
- Static analysis to determine variable signs at each program point
- Abstract interpretation framework
- Chaotic iteration algorithm for fixpoint computation
- Detects potential division-by-zero errors before execution
- Supports both positive, negative, and zero sign abstract values

### 6. **Security Analysis**
- Information flow security analysis
- Lattice-based security classifications
- Detects implicit and explicit information flows
- Identifies security violations
- Computes allowed vs. actual flows

## Architecture

The project follows a classic compiler pipeline architecture:

```
Source Code → Lexer → Parser → AST → Program Graph → Analysis/Execution
```

Each component is modular and can be used independently:
- **Frontend**: Lexer + Parser → AST
- **Middle-end**: AST → Program Graph generation
- **Backend**: Interpretation, verification, and analysis passes

## Components

### Core Language Implementation

**[AST.fs](AST.fs)** - Abstract Syntax Tree definitions
- Defines data types for arithmetic expressions, boolean expressions, commands, and guarded commands
- Foundation for all subsequent program transformations

**[Lexer.fsl](Lexer.fsl)** - Lexical analyzer specification
- Tokenizes source code using regular expressions
- Handles keywords, operators, numbers, booleans, and identifiers

**[Parser.fsy](Parser.fsy)** - Parser specification
- Context-free grammar for GCL
- Precedence and associativity rules
- Constructs AST from token stream

**[Parse.fs](Parse.fs)** - Parser interface and pretty printing
- Wraps FsLex/FsYacc generated code
- Implements pretty printing for source code regeneration
- Error handling and reporting

### Program Analysis & Execution

**[Graph.fs](Graph.fs)** - Program graph generation
- Compiles AST to control flow graph representation
- Implements edges with labeled transitions
- Supports both deterministic and non-deterministic semantics
- Generates DOT notation for visualization

**[Interpreter.fs](Interpreter.fs)** - Program execution engine
- Implements operational semantics for GCL
- Arithmetic and boolean expression evaluation
- Memory state management (variables and arrays)
- Execution sequence generation
- Stuck state detection

**[SignAnalysis.fs](SignAnalysis.fs)** - Static sign analysis
- Abstract interpretation framework
- Kill/gen analysis for sign propagation
- Chaotic iteration for fixpoint computation
- Detects potential runtime errors (division by zero)

**[ProgramVerification.fs](ProgramVerification.fs)** - Formal verification
- Strongest postcondition generation
- Verification condition generation
- Predicate substitution and manipulation
- Support for quantified formulas

**[Security.fs](Security.fs)** - Information flow analysis
- Computes actual information flows (explicit and implicit)
- Lattice-based security model
- Flow violation detection
- Reflexive and transitive closure computation

### Infrastructure

**[Program.fs](Program.fs)** - Main entry point
- Command-line interface
- JSON serialization/deserialization
- Dispatches to appropriate analysis module

**[Types.fs](Types.fs)** - Shared type definitions
- Common types used across modules

## Technology Stack

- **Language**: F# (functional-first .NET language)
- **Lexer Generator**: FsLex
- **Parser Generator**: FsYacc
- **Framework**: .NET 7.0
- **Build System**: .NET CLI
- **Serialization**: Newtonsoft.Json

## Usage

### Build the project
```bash
dotnet build
```

### Run analyses

**Parse and pretty print:**
```bash
dotnet run parse "x := 5; y := x + 3"
```

**Generate program graph:**
```bash
dotnet run graph "if x > 0 -> y := 1 [] x <= 0 -> y := -1 fi" '{"determinism":{"Case":"NonDeterministic"}}'
```

**Execute program:**
```bash
dotnet run interpreter "x := 5; y := x * 2" '{"determinism":{"Case":"NonDeterministic"},"assignment":{"variables":{},"arrays":{}},"trace_length":100}'
```

**Sign analysis:**
```bash
dotnet run sign "x := -5; if x > 0 -> y := 1 [] x <= 0 -> y := -1 fi" '{"determinism":{"Case":"NonDeterministic"},"assignment":{"variables":{"x":["Negative"]},"arrays":{}}}'
```

**Security analysis:**
```bash
dotnet run security "h := l; l := h" '{"lattice":[{"from":"low","into":"high"}],"classification":{"variables":{"h":"high","l":"low"},"arrays":{}}}'
```

## Project Structure

```
.
├── AST.fs                      # Abstract syntax tree definitions
├── Lexer.fsl                   # Lexer specification
├── Lexer.fs                    # Generated lexer
├── Parser.fsy                  # Parser grammar
├── Parser.fs/fsi               # Generated parser
├── Parse.fs                    # Parser interface & pretty printer
├── Graph.fs                    # Program graph construction
├── Interpreter.fs              # Program interpreter
├── SignAnalysis.fs             # Sign analysis implementation
├── ProgramVerification.fs      # Verification condition generator
├── Security.fs                 # Information flow analysis
├── Program.fs                  # Main entry point
├── Types.fs                    # Shared type definitions
├── calculator.fsproj           # Project file
├── run.toml                    # Build configuration
└── predicates/                 # Predicate logic support
    ├── AST.fs
    ├── Lexer.fsl
    ├── Parser.fsy
    └── Parse.fs
```

