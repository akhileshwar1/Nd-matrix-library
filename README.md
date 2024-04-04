# N-Dimensional Matrix Library

This library is designed to handle n-dimensional matrices using list of lists representations. It offers functionalities for basic matrix operations like multiplication, addition, and more, with a special focus on n-dimensional matrices.

## Definition of N-dimensional Matrix

An n-dimensional (nd) matrix is defined recursively in terms of (n-1)d matrices. For example:

- A 2d matrix is a list of 1d matrices (lists) combined with another 2d matrix.
- A 3d matrix is a 2d matrix combined with a 3d matrix.

This recursive definition extends to all n-dimensional matrices.

## Operations

Operations on matrices are also defined recursively. For example, for multiplication:

- 3d X = 2d X + 3d X (Here, X represents multiplication).

## Representation of Matrices

Matrices are represented using built-in vector data structures to provide constant access and update times.

## Grammar and Functionalities

### Creating a Matrix

- **Syntax**: `(matrix name n1 n2 ...) -> matrix?`
- **Usage**: Creates an empty matrix with dimensions specified by `n1`, `n2`, etc.

### Accessing Elements in a Matrix

- **Syntax**: `(matrix-ref m pos1 pos2 ...) -> number?`
- **Usage**: Accesses the element/matrix at specified positions in matrix `m`.

### Updating Elements in a Matrix

- **Syntax**: `(matrix-update m n pos1 pos2 ...) -> number?`
- **Usage**: Updates the element/matrix in `m` at the specified positions.

### Multiplying Matrices

- **Syntax**: `(matrix-mult m1 m2) -> matrix?`
- **Usage**: Multiplies matrices `m1` and `m2` using dot products.

### Creating a Matrix from a List

- **Syntax**: `(list->matrix lst dims name)`
- **Usage**: Creates a matrix from a list `lst` with dimensions `dims`.
