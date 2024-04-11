# Sudoku: The Elegance of Language-Oriented Programming in Scheme

There are certain languages that offer not just tools for creating software but a canvas for expressing ideas elegantly and with incredible precision. Among these, Scheme—a dialect of Lisp—stands out for its unique combination of simplicity and power. Its powerful macros and first-class functions enable superb dexterity in functional and language-oriented programming, where the language itself can be molded and stretched to fill the problem at hand. This blog post explores this beautiful aspect of Scheme through the lens of a backtracking Sudoku solver, demonstrating the language's ability to describe a solution to such a nuanced problem.

Scheme, with its minimalist syntax and powerful macro system, encourages a different way of thinking about programming. It's not just about writing code; it's about designing a language (a domain-specific language) tailored to the specific problem you are trying to solve. This approach is particularly helpful when solving complex problems that can be described through the composition of simpler pieces. Sudoku, a logic-based number-placement puzzle, itself presents an intriguing problem: fill a 9x9 grid so that each column, each row, and each of the nine 3x3 subgrids contain all of the digits from 1 to 9.

## A Backtracking Sudoku Solve

Let's dive into one implementation of a Sudoku solver in Scheme. The `solve-board` function that implements the heart of the backtracking algorithm is shown below. Scheme's ability to mold and extend the language allows the algorithm to be define as a description of what it does. Much of what it describes can be understood without much familiarity with the language. The code looks for the first empty position (`hole`) on the board. If it doesn't fine a hole, the board is already solved.

If it does finds a hole, for the first of the valid candidates values (`valid-values`) it checks to see if that value (`candidate`) is allowed (`candidate-allowed?`) in the hole according to the rules of the game. If the value is allowed, it fills the hole with the candidate value (`set-pos-value`). Then function recursively invokes itself on a new version of the board with the hole filled. If that recursive path succeeds (`board-solved?`), we have a solution, and we return the solved board using a continuation. But if that path fails, we backtrack and try the next valid candidate in the hole. If all valid candidates fail, no solution exists.

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

The definitions of the helper functions used in `solve-board` are available on [GitHub](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.rkt). A visualization of the backtracking algorithm in action solving a Sudoku board is shown below.

( backtracking video or gif )

## Conclusion

Hopefully, through the lens of this backtracking Sudoku solver, you can appreciate some of the elegance and power of language-oriented programming as a tool for solving complex problems. Look through the code on GitHub and see what you think.
