# Sudoku: The elegance of language-oriented programming in Scheme

There are certain languages that offer not just tools for creating software but a canvas for expressing ideas elegantly and with incredible precision. Among these, Scheme—a dialect of Lisp—stands out for its unique combination of simplicity and power. Its macros and first-class functions allow superb dexterity in functional and language-oriented programming, where the language itself can be molded and stretched to fill the problem at hand. This blog post explores this beautiful aspect of Scheme through the lens of a backtracking Sudoku solver, demonstrating the language's ability to describe a solution to such a nuanced problem.

Scheme, with its minimalist syntax and powerful macro system, encourages a different way of thinking about programming. It's not just about writing code; it's about designing a language (a domain-specific language) tailored to the specific problem you are trying to solve. This approach is particularly helpful when solving complex problems that can be described through the composition of simpler pieces. Sudoku, a logic-based number-placement puzzle, itself presents an intriguing problem: fill a 9x9 grid so that each column, each row and each of the nine 3x3 subgrids contain all of the digits from 1 to 9.

First-class functions allow languages like Scheme to treat functions as data, meaning they can be passed as arguments to other functions, returned as values from functions, and assigned to variables. This flexibility is crucial for creating high-level abstractions and manipulating the code itself as data. This enables more expressive and concise solutions to complex problems, like navigating the possibilities in a Sudoku puzzle through recursion and the use of higher-order functions.

And without getting into detail, macros are "programs that write programs" (Doug Hoyte, Let Over Lambda, 2008). Macros let you manipulate and extend the language on a higher plane. The language is superbly flexible. It's like working with clay. An example, you can write a recursive interpreter for the language elegantly in the language. It "eats itself".

## A backtracking solver

Let's dive into one implementation of a Sudoku solver in Scheme. The `solve-board` function that implements the heart of the backtracking algorithm is shown below. Scheme's ability to mold and extend the language allows the algorithm to be defined as a description of what it does. Much of what it describes can be understood without much familiarity with the language. The code looks for the position of the first empty cell (hole) on the board (`get-first-hole-pos`). If it doesn't find a hole, the board is already solved.

Upon finding a hole, it iterates over valid candidate values (`valid-values`), checking if they can be placed in the hole according to the rules of the game (`candidate-allowed?`). If the candidate is allowed, it fills the hole (`set-pos-value`). Then the function recursively invokes itself on a new version of the board with the hole filled. If the recursive path succeeds (`board-solved?`), we have a solution. Return the solved board using a continuation. If the path we're in fails (returns an empty list instead of a solved board), we backtrack and try the next valid candidate. If all candidates fail, no solution exists.

Look through the code and decide how well it describes what happens.

( TODO - use screenshots instead of code blocks to ensure rainbow parens and aesthetic theme and typeface )

```scheme
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

## Some helper functions

The Sudoku board is modeled here as a simple one-dimensional list with either valid cell values (1-9) or 0s indicating a hole.

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
```

Maths are used to determine which row, column, or subgrid (box) any position on the board belongs to. This is calculated in the `get-row`, `get-col`, and `get-box` helper functions, respectively.

```scheme
(define (get-row pos)
  (floor (/ pos 9)))

(define (get-col pos)
  (modulo pos 9))

(define (get-box pos)
  (let ((row (get-row pos))
        (col (get-col pos)))
    (+ (* (floor (/ row 3)) 3)
       (floor (/ col 3)))))
```

Mapping each of the index positions for the board to the corresponding `get-[xxx]` helpers helps visualize how their behavior. The mapping of the `get-box` function is shown as an example below. The output can be seen in the comment block below the code. Note that the `range` function is used here to returns a list of numbers from 0 through 80 (the indices of each cell on the board).

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
(define (get-first-hole-pos board)
  (index-of board 0))
```

## Encoding the rules

The rules of the game are encoded in the `candidate-allowed?` predicate function. The function looks at the values in the row, column, and box that correspond to a specific position, and determines if a candidate (proposed) value is already in use. `candidate-allowed?` uses `get-forbidden-candidates`, a function that returns a list of all of values already being used (in the same row, column, or box) for a specific position on the board. The code for `candidate-allowed?` and `get-forbidden-candidates` (and an example of one of the `get-[xxx]-values` functions) are copied below. Notice each one is concise.

```scheme
(define (candidate-allowed? board pos value)
  (not (member value (get-forbidden-candidates board pos))))

(define (get-forbidden-candidates board pos)
  (remove-duplicates
   (append (get-row-values board (get-row pos))
           (get-col-values board (get-col pos))
           (get-box-values board (get-box pos)))))

(define (get-row-values board row)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= row (get-row index))))
                         (zip-with-index board))))
    (map cadr matching-pairs)))
```

## Conclusion
I hope by walking through this backtracking Sudoku solver, you have come to get a glance of not just the elegance and power of a language-oriented approach to programming, but also how it can be used to solve complex problems with clarity and precision. Scheme, with its minimalist syntax and profound capabilities, isn't just a programming language—it's a way of thinking and reasoning about a problem, a method of directly expressing solutions in a manner as logical and nuanced as the problem you're solving.

The full code for the algorithm is available on [GitHub](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.scm). You can use [Racket](https://racket-lang.org/) to run the code.

Look through the code on GitHub. I'd love to hear what you think. Can you see this flexibility leading to cleaner and more effective code? You can watch the algorithm working below.

![backtracking](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.gif?raw=true)
