# Homework 00: Binary Numbers in Haskell

The focus of this assignment is to model binary numbers in Haskell using recursive data structures and to implement fundamental operations on them.

## Core Concepts

### `Bit`
Represents a single binary digit. Although similar to standard boolean values, `Bit` is strictly intended for data representation rather than control flow control.

### `Binary`
A recursive data type defining binary numbers.
*   `End` signifies the termination of a binary sequence.
*   The `(:.)` operator is used to extend a binary number from the right (e.g., `End :. One :. Zero`). 
*   *Note:* The `(:.)` constructor is left-associative.

Because this structure allows appending infinite leading zeros, numbers can have multiple valid representations. A **Canonical `Binary`** is defined as a binary number with absolutely no leading zeros.

## Implemented Functions

The primary logic is located in `src/Binary.hs` and includes the following operations:

*   **`succBinary`**: Increments a given binary number by 1.
*   **`integerToBinary`**: Converts a non-negative integer to its canonical binary representation.
*   **`binaryToInteger`**: Converts a given binary number back to a standard integer.
*   **`hasLeadingZero`**: A helper function to check for the presence of leading zeros.
*   **`isEnd`**: A simple check to determine if the binary sequence is `End`.
*   **`canonicalise`**: Converts any binary number to its canonical form (stripping leading zeros).
*   **`addBinary`**: Implements binary addition for two `Binary` values.

*(Note: The implementation of later functions strictly avoids using the integer conversion functions to ensure pure binary logic is applied.)*

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

*(Note: The `Can.hs` file contains a partial implementation of a "Correct by Construction" approach intended as a bonus exercise, focusing on types that guarantee canonical representation without the need for manual validation.)*