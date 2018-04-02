#lang racket
; takes a list of numbers, operators in post fix notation
(define (evalPostFix expr)
  (do ((remaining expr (cdr remaining))
       (resultList '() (if (char? (car remaining))
                            (cons (eval (car remaining) (car resultList) (cadr resultList)) (cddr resultList))
                            (cons (car remaining) resultList))))
    ((null? remaining) (car resultList))))


(define (eval opp l r)
  (case opp
    [(#\+) (+ l r)]
    [(#\-) (- l r)]
    [(#\/) (/ l r)]
    [(#\*) (* l r)]))

;(evalPostFix '(15 7 1 1 #\+ #\- #\/ 3 #\* 2 1 1 #\+ #\+ #\-)) ; Test = 5