#lang racket

; Takes a list of format '(number conditional number) e.g. '(2 "<" 4)
; Conditionals are strings
(define (booleanEval expr)
  (case (cadr expr)
    [("==") (eq? (car expr) (car (cddr expr)))]
    [("<>") (not (eq? (car expr) (car (cddr expr))))]
    [(">=") (>= (car expr) (car (cddr expr)))]
    [("<=") (<= (car expr) (car (cddr expr)))]
    [(">") (> (car expr) (car (cddr expr)))]
    [("<") (< (car expr) (car (cddr expr)))]))



