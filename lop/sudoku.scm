#lang scheme


;;   board - [int] (length 81)
;;   pos - int (0-80)
;;   row - int (0-8)
;;   col - int (0-8)
;;   box - int (0-8)
;;   value - int (1-9)


(define unsolved-board
  (list 0 0 0 0 6 2 3 0 0
        3 4 9 0 1 0 7 0 0
        0 5 0 4 3 0 0 0 1
        0 0 2 6 5 0 0 0 9
        0 0 8 0 4 0 1 6 2
        0 6 4 2 9 1 0 3 8
        0 0 0 0 0 6 0 0 0
        0 8 0 0 7 0 0 5 4
        9 0 0 3 2 0 6 0 7 ))


(define valid-values '(1 2 3 4 5 6 7 8 9))


;; backtracking solver
;; solve-board :: board -> board
;;             :: [int] -> [int]  (empty list if no solution found)
(define (solve-board board)
  (call-with-current-continuation
   (lambda (return)
     (let ((hole (get-first-hole-pos board)))
       (if (not hole)
           board ; return solved board
           (begin
             (for ((candidate valid-values))
               (when (candidate-allowed? board hole candidate)
                 (let ((could-be-solution (solve-board ; recursive call
                                           (set-pos-value
                                            board
                                            hole
                                            candidate))))
                   (when (board-solved? could-be-solution)
                     (return could-be-solution))))) ; return solved board
             '())))))) ; all candidates exhausted - no solution found


;; display-board :: board -> null (impure)
;;               :: [int] -> null
(define (display-board board)
  (newline)
  (let ((display-row (lambda (row)
                       (for-each
                        (lambda (value)
                          (display (format " ~a " value)))
                        (get-row-values board row))
                       (newline))))
    (for-each display-row (range 9)))
  (newline))


;; get-pos-value :: board -> pos -> value
;;               :: [int] -> int -> -> int
(define get-pos-value list-ref) ; list-ref ( point-free )


;; set-pos-value :: board -> pos -> value -> board
;;               :: [int] -> int -> -> int -> [int]
(define (set-pos-value board pos value)
  (append (take board pos)
          (list value)
          (drop board (add1 pos))))


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


;; zip :: [T] -> [U] -> ... -> [T U ...]
(define (zip lst . lsts)
  (apply map list lst lsts))


;; zip-with-index :: [T] -> [int T ...]
(define (zip-with-index lst)
  (zip (range (length lst)) lst))


;; get-first-hole-pos :: board -> pos
;;                    :: [int] -> int  (returns #f no holes are found)
;; note: get-first-hole-pos returns #f no empty positions are found
(define (get-first-hole-pos board)
  (index-of board 0))


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


;; get-col-values :: board -> col -> [values]
;;                :: [int] -> int -> [int]
(define (get-col-values board col)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= col (get-col index))))
                         (zip-with-index board))))
    (map cadr matching-pairs)))


;; get-box-values :: board -> box -> [values]
;;                :: [int] -> int -> [int]
(define (get-box-values board box)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= box (get-box index))))
                         (zip-with-index board))))
    (map cadr matching-pairs)))


;; board-solved? :: board -> boolean
;;               :: [int] -> boolean
(define (board-solved? board)
  (and (not (null? board)) ; board is not null
       (not (get-first-hole-pos board)))) ; board contains no empty positions



(display-board (solve-board unsolved-board))
