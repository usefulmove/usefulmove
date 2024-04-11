# Sudoku: The Elegance of Language-Oriented Programming in Scheme

There are certain languages that offer not just tools for creating software but a canvas for expressing ideas elegantly and with incredible precision. Among these, Scheme—a dialect of Lisp—stands out for its unique combination of simplicity and power. Its macros and first-class functions allow superb dexterity in functional and language-oriented programming, where the language itself can be molded and stretched to fill the problem at hand. This blog post explores this beautiful aspect of Scheme through the lens of a backtracking Sudoku solver, demonstrating the language's ability to describe a solution to such a nuanced problem.

Scheme, with its minimalist syntax and powerful macro system, encourages a different way of thinking about programming. It's not just about writing code; it's about designing a language (a domain-specific language) tailored to the specific problem you are trying to solve. This approach is particularly helpful when solving complex problems that can be described through the composition of simpler pieces. Sudoku, a logic-based number-placement puzzle, itself presents an intriguing problem: fill a 9x9 grid so that each column, each row, and each of the nine 3x3 subgrids contain all of the digits from 1 to 9.

## A Backtracking Sudoku Solver

Let's dive into one implementation of a Sudoku solver in Scheme. The `solve-board` function that implements the heart of the backtracking algorithm is shown below. Scheme's ability to mold and extend the language allows the algorithm to be defined as a description of what it does. Much of what it describes can be understood without much familiarity with the language. The code looks for the position of the first empty cell (hole) on the board (`get-first-hole-pos`). If it doesn't find a hole, the board is already solved.

Upon finding a hole, it iterates over valid candidate values (`valid-values`), checking if they can be placed in the hole according to the rules of the game (`candidate-allowed?`). If the candidate is allowed, it fills the hole (`set-pos-value`). Then the function recursively invokes itself on a new version of the board with the hole filled. If that recursive path succeeds (`board-solved?`), we have a solution, and we return the solved board using a continuation. But if that path fails, we backtrack and try the next valid candidate in the hole. If all valid candidates fail, no solution exists.

( TODO - use screenshots instead of code blocks to ensure rainbow parens and aesthetic theme and typeface )

```scheme
;; solve-board :: board -> board
;;             :: [int] -> [int] (empty list if no solution found)
(define (solve-board board)
  (call-with-current-continuation
   (lambda (return)
     (let ((hole (get-first-hole-pos board)))
       (if (not hole)
           board ; return solved board.
           (begin
             (for ((candidate valid-values))
               (when (candidate-allowed? board hole candidate)
                 (let ((could-be-solution (solve-board
                                           (set-pos-value
                                            board
                                            hole
                                            candidate))))
                   (when (board-solved? could-be-solution)
                     (return could-be-solution))))) ; return solved board.
             '())))))) ; all candidates exhausted. no solution found.
```

## Supporting Functions

The Sudoku board is modeled here as a simple one-dimensional list with either valid cell values (1-9) or 0s indicating a hole, and maths are used to determine to which row, column, or subgrid (box) the one-dimensional index into the board belongs using the `get-row`, `get-col`, and `get-box` helper functions.

```scheme
(define unsolved-board
  (list 5 3 0 0 7 0 0 0 0
        6 0 0 1 9 5 0 0 0
        0 9 8 0 0 0 0 6 0
        8 0 0 0 6 0 0 0 3
        4 0 0 8 0 3 0 0 1
        7 0 0 0 2 0 0 0 6
        0 6 0 0 0 0 2 8 0
        0 0 0 4 1 9 0 0 5
        0 0 0 0 8 0 0 7 9 ))

;; get-row :: pos -> row
;;         :: int -> int
(define (get-row pos)
  (floor (/ pos 9)))

;; get-col :: pos -> col
;;         :: int -> int
(define (get-col pos)
  (modulo pos 9))

;; get-box :: pos -> box
;;         :: int -> int
(define (get-box pos)
  (let ((row (get-row pos))
        (col (get-col pos)))
    (+ (* (floor (/ row 3)) 3)
       (floor (/ col 3)))))
```

Mapping each of the index positions for the board to the corresponding `get-xxx` helpers helps visualize how their behavior. The function output is shown below as comment block below the code. (Note that the `range` function is used here. When passed a single positive integer argument, it returns a list from 0 through one less than the argument passed. It is used here to build a list of all of the index positions of the Sudoku board.)

```scheme
(map get-row (range (length unsolved-board)))
#|
    0 0 0 0 0 0 0 0 0
    1 1 1 1 1 1 1 1 1
    2 2 2 2 2 2 2 2 2
    3 3 3 3 3 3 3 3 3
    4 4 4 4 4 4 4 4 4
    5 5 5 5 5 5 5 5 5
    6 6 6 6 6 6 6 6 6
    7 7 7 7 7 7 7 7 7
    8 8 8 8 8 8 8 8 8
|#
```

```scheme
(map get-col (range (length unsolved-board)))
#|
    0 1 2 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
    0 1 3 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
    0 1 2 3 4 5 6 7 8
|#
```

```scheme
(map get-box (range (length unsolved-board)))
#|
    0 0 0 1 1 1 2 2 2
    0 0 0 1 1 1 2 2 2
    0 0 0 1 1 1 2 2 2
    3 3 3 4 4 4 5 5 5
    3 3 3 4 4 4 5 5 5
    3 3 3 4 4 4 5 5 5
    6 6 6 7 7 7 8 8 8
    6 6 6 7 7 7 8 8 8
    6 6 6 7 7 7 8 8 8
|#
```

Finding the first hole position is done by searching for the first 0 in the list (board).

```scheme
;; get-first-hole-pos :: board -> pos
;;                    :: [int] -> int  (returns #f no holes are found)
(define (get-first-hole-pos board)
  (index-of board 0))
```

The rules of the game are encoded in the `candidate-allowed?` predicate function. The function looks at all of the values in the row, column, and box that correspond to the position on the board passed to it, and determines if the proposed value is already in use. `candidate-allowed` calls a `get-forbidden-candidates` function that returns a list of all of the values already in use (in the same row, column, and box) for the board position provided. The code for `candidate-allowed?`, `get-forbidden-values`, and an example of one of the `get-[xxx]-values` functions used in their definition are copied below. You can use Racket to run the code.

```scheme
;; candidate-allowed? :: board -> pos -> value -> boolean
;;                    :: [int] -> int -> int -> boolean
(define (candidate-allowed? board pos value)
  (not (member value (get-forbidden-candidates board pos))))

;; get-forbidden-candidates :: board -> pos -> [value]
;;                          :: [int] -> int -> [int]
(define (get-forbidden-candidates board pos)
  (remove-duplicates
   (append (get-row-values board (get-row pos))
           (get-col-values board (get-col pos))
           (get-box-values board (get-box pos)))))

;; get-row-values :: board -> row -> [values]
;;                :: [int] -> int -> [int]
(define (get-row-values board row)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= row (get-row index))))
                         (zip-with-index board))))
    (map cadr matching-pairs)))
```

The full code for the backtracking solve is available on [GitHub](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.scm).

## Conclusion
I hope by walking through this backtracking Sudoku solver, you have come to get a glance of not just the elegance and power of a language-oriented approach to programming, but also how it can be used to solve complex problems with clarity and precision. Scheme, with its minimalist syntax and profound capabilities, isn't just a programming language—it's a way of thinking and reasoning about a problem, a method of directly expressing solutions in a manner as logical and nuanced as the problem you're solving.

Look through the code on GitHub. I'd love to know what you think. You can see the algorithm working below.

( TODO - insert backtracking video or gif )