# Homework 01: Functional Matrices & Tic-Tac-Toe

The focus of this assignment is to build a custom Matrix library using higher-order functions and to apply that library to implement the evaluation logic for a Tic-Tac-Toe game.

## Part 1: Higher-Order Function Matrices (`Matrix.hs`)

Instead of using standard lists of lists, this implementation defines a matrix as a function that maps indices to values. This approach enforces a strict 3x3 dimension at the type level and guarantees row and column length safety.

### Core Concepts

*   **`Three`**: A custom data type with exactly three values (`Zero`, `One`, `Two`), used as safe, finite indices.
*   **`Thrice a`**: A type synonym representing a "row" of three elements, defined as a function `Three -> a`.
*   **`Matrix a`**: The core matrix type, representing a 3x3 grid as a function `Thrice (Thrice a)` (or `Three -> Three -> a`).

### Implemented Matrix Operations

The custom library includes several standard matrix operations implemented through function composition and higher-order functions:

*   **Constructors**: `constantMatrix`, `diagonalMatrix`, `otherDiagonalMatrix`.
*   **Accessors**: `ix`, `getRow`, `getCol`, `getDiag`, `getOtherDiag`.
*   **Transformations**: `transpose`, `addMatrix`, `place` (modifying a specific cell).
*   **Mapping & Folding**: `mapMatrix`, `imapMatrix`, `foldMatrixWith`.
*   **String Formatting**: `concatMatrixWith`, `showMatrixComposition`.

## Part 2: Tic-Tac-Toe Logic (`TicTacToe.hs`)

Using the custom `Matrix` library, this module implements the rules for evaluating a Tic-Tac-Toe game state.

### Core Concepts

*   **`Marker` & `Spot`**: Defines the players (`X` and `O`) and the state of a single cell on the board (`Maybe Marker`).
*   **`Board`**: A type synonym for `Matrix Spot`, representing the game grid.
*   **`Result`**: Represents the outcome of checking a row, column, diagonal, or the entire board (`Full`, `HasEmpty`, or `Wins Marker`).

### Implemented Functions

*   **`checkThreeSpots`**: Evaluates a single `Thrice Spot` (a row, column, or diagonal) for a winner.
*   **`join`**: Combines multiple `Result` values to determine the overall game state.
*   **`winner`**: Checks all rows, columns, and diagonals to declare an overall winner, a tie, or an ongoing game.
*   **`emptySpots`**: Returns the coordinates of all unplayed cells on the board.

## Running the Tests

To test the implementation locally, you can use the Cabal build tool.

*   Run the test suite:
    ```bash
    cabal test
    ```
*   Load the modules interactively for debugging:
    ```bash
    cabal repl
    ```