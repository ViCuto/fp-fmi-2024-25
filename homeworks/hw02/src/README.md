# Homework 02: AST Evaluation, Transpilation & Optimization

This assignment focuses on extending an Abstract Syntax Tree (AST) for a simple expression language, implementing an interpreter with variable contexts, building a transpiler to Racket (Lisp), and writing an optimizer for constant folding.

## Part 1: Interpretation & Variable Contexts

The base `Expr` language evaluates mathematical expressions and conditionals. It has been extended to support variables (`Var`) and context-dependent execution.

### Core Concepts

*   **`Expr` Data Type**: Represents the AST with nodes for values (`Val`), variables (`Var`), binary operations (`Oper`), conditionals (`If`), lists of sums (`SumList`), and bounded iterations (`Sum`).
*   **`Context`**: A mapping of variable names (strings) to `Integer` values, passed during evaluation to resolve free variables.
*   **Operations**: Abstracted via `OperType` to support `Plus` and `Mult` dynamically.

### Implemented Functions

*   **`eval`**: An interpreter that takes a `Context` and an `Expr`, safely resolving variables and evaluating the tree (returning `Maybe Integer` to handle unbound variables).
*   **`freeVars`**: Traverses an AST to find and return a list of all unbound variables.
*   **Context Helpers**: Custom monadic-style combinators (`maybeAndThen`, `traverseListMaybe`) for safe execution, along with a `lookup` function for variable resolution.

## Part 2: Compilation to Racket

This part implements a transpiler that converts the custom `Expr` language into valid Racket (Scheme/Lisp) code, specifically leveraging S-expressions.

### Implemented Functions

*   **`RacketExpr` & `RacketProgram`**: Custom types defining the structure of Lisp-like expressions (`Name` and `List`).
*   **`compileToRacket`**: Recursively translates an `Expr` tree into a `RacketExpr` structure.
*   **`printRacketProgram`**: Formats the compiled S-expressions into executable string output, automatically injecting variable definitions from a given `Context` and adding the `#lang racket` header.

## Part 3: Partial Evaluation (Constant Folding)

An optimization step designed to simplify the AST before interpretation or compilation.

### Implemented Functions

*   **`partialEval`**: Recursively traverses the AST to identify subtrees containing only known constants. It pre-evaluates arithmetic operations, resolves static `If` branches, and flattens `SumList` nodes where possible, returning a simplified `Expr` tree without altering program semantics.

---
*(Note: The bounded iteration constructor `Sum` is fully supported by the interpreter, but its transpilation and partial evaluation were intended as an optional bonus and are intentionally not implemented.)*

## Running the Tests

To test the implementation locally, you can use the Cabal build tool.

*   Run the test suite:
    ```bash
    cabal test
    ```
*   Load the module interactively for debugging:
    ```bash
    cabal repl
    ```