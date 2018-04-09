#lang racket
(provide evalPostFix)

; takes a list of numbers, operators in post fix notation
(define (evalPostFix expr)
  (do ((remaining expr (cdr remaining))
       (resultList '(0) (if (char? (car remaining))
                            (cons (eval (car remaining) (car resultList) (cadr resultList)) (cddr resultList))
                            (cons (car remaining) resultList))))
    ((null? remaining) (car resultList))))


(define (eval opp r l)
  (case opp
    [(#\+) (+ l r)]
    [(#\-) (- l r)]
    [(#\/) (/ l r)]
    [(#\*) (* l r)]
    [(#\!) (factorial r)]))

(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

;(evalPostFix '(15 7 1 1 #\+ #\- #\/ 3 #\* 2 1 1 #\+ #\+ #\-)) ; Test = 5