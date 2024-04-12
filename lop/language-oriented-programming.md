# Sudoku: The elegance of language-oriented programming in Scheme

There are certain languages that offer not just tools for creating software but a canvas for expressing ideas elegantly and with incredible precision. Among these, Scheme—a dialect of Lisp—stands out for its unique combination of simplicity and power. Its macros and first-class functions allow superb dexterity in functional and language-oriented programming, where the language itself can be molded and stretched to fill the problem at hand. This blog post explores this beautiful aspect of Scheme through the lens of a backtracking Sudoku solver, demonstrating the language's ability to describe a solution to such a nuanced problem.

Scheme, with its minimalist syntax and powerful macro system, encourages a different way of thinking about programming. It's not just about writing code; it's about designing a language (a domain-specific language) tailored to the specific problem you are trying to solve. This approach is particularly helpful when solving complex problems that can be described through the composition of simpler pieces. Sudoku, a logic-based number-placement puzzle, itself presents an intriguing problem: fill a 9x9 grid so that each column, each row and each of the nine 3x3 subgrids contain all of the digits from 1 to 9.

First-class functions allow languages like Scheme to treat functions as data, meaning they can be passed as arguments to other functions, returned as values from functions, and assigned to variables. This flexibility is crucial for creating high-level abstractions and manipulating the code itself as data. This enables more expressive and concise solutions to complex problems, like navigating the possibilities in a Sudoku puzzle through recursion and the use of higher-order functions.

And without getting into detail, macros are "programs that write programs" (Doug Hoyte, Let Over Lambda, 2008). Macros let you manipulate and extend the language on a higher plane. The language is superbly flexible. It's like working with clay. An example of this flexibility is that you can write a recursive interpreter for the language elegantly within the language itself. It "eats itself" (see [metacircular evaluator](https://sarabander.github.io/sicp/html/4_002e1.xhtml#g_t4_002e1), Structure and Interpretation of Computer Programs, Abelson and Sussman, 1984).

## A backtracking solver

Let's dive into one implementation of a Sudoku solver in Scheme. The `solve-board` function that implements the heart of the backtracking algorithm is shown below. Scheme's ability to mold and extend the language allows the algorithm to be defined as a description of what it does. Much of what it describes can be understood without much familiarity with the language. The code looks for the position of the first empty cell (hole) on the board (`get-first-hole-pos`). If it doesn't find a hole, the board is already solved.

Upon finding a hole, it iterates over valid candidate values (`valid-values`), checking if they can be placed in the hole according to the rules of the game (`candidate-allowed?`). If the candidate is allowed, it fills the hole (`set-pos-value`). Then the function recursively invokes itself on a new version of the board with the hole filled. If the recursive path succeeds (`board-solved?`), we have a solution. Return the solved board using a continuation. If the path we're in fails (returns an empty list instead of a solved board), we backtrack and try the next valid candidate. If all candidates fail, no solution exists.

Look through the code and decide how well you think it describes what it does.

The full implementation is available on [GitHub](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.scm).

![solve-board](https://github.com/usefulmove/usefulmove/blob/main/lop/solve-board.png?raw=true)

## Some helper functions

The Sudoku board is modeled here as a simple one-dimensional list with either valid cell values (1-9) or 0s indicating a hole.

![solve-board](https://github.com/usefulmove/usefulmove/blob/main/lop/unsolved-board.png?raw=true)

Maths are used to determine which row, column, or subgrid (box) any position on the board belongs to. This is calculated in the `get-row`, `get-col`, and `get-box` helper functions, respectively.

![solve-board](https://github.com/usefulmove/usefulmove/blob/main/lop/get-xxx.png?raw=true)

Mapping each of the index positions for the board to the corresponding `get-[xxx]` helpers helps visualize how their behavior. The mapping of the `get-box` function is shown as an example below. The output can be seen in the comment block below the code. Note that the `range` function is used here to returns a list of numbers from 0 through 80 (the indices of each cell on the board).

![solve-board](https://github.com/usefulmove/usefulmove/blob/main/lop/get-box-map.png?raw=true)

Finding the first hole position is done by searching for the first 0 in the list (board).

![solve-board](https://github.com/usefulmove/usefulmove/blob/main/lop/get-first-hole-pos.png?raw=true)

## Encoding the rules

The rules of the game are encoded in the `candidate-allowed?` predicate function. The function looks at the values in the row, column, and box that correspond to a specific position, and determines if a candidate (proposed) value is already in use. `candidate-allowed?` uses `get-forbidden-candidates`, a function that returns a list of all of values already being used (in the same row, column, or box) for a specific position on the board. The code for `candidate-allowed?` and `get-forbidden-candidates` (and an example of one of the `get-[xxx]-values` functions) are copied below. Notice each one is concise.

![solve-board](https://github.com/usefulmove/usefulmove/blob/main/lop/candidate-allowed.png?raw=true)

## Conclusion
I hope that by walking through this solution, you have a sense of not just the elegance and power of a language-oriented approach, but also a sense of how it can be used to solve complex problems with clarity and precision. Scheme, with its minimalist syntax and incredible flexibility, isn't just a language—it becomes a way of reasoning about and describing a solution your problem. It allows you to more directly express a solution in a manner just as nuanced as the problem you're solving.

The code for the algorithm is available on [GitHub](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.scm). You can use [Racket](https://racket-lang.org/) to run it and make changes. Look through the code, and let me know what you think. Can you see this approach leading to better code? Do you have another implementation (in another language?) that you want to share?

You can see the algorithm in action below.

![backtracking](https://github.com/usefulmove/usefulmove/blob/main/lop/sudoku.gif?raw=true)
