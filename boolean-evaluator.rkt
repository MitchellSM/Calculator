#lang racket
(provide evalBoolean)


; Takes a list of format '(number conditional number) e.g. '(2 "<" 4)
; Conditionals are strings
(define (evalBoolean expr)
  (case (cadr expr)
    [("==") (eq? (car expr) (car (cddr expr)))]
    [("<>") (not (eq? (car expr) (car (cddr expr))))]
    [(">=") (>= (car expr) (car (cddr expr)))]
    [("<=") (<= (car expr) (car (cddr expr)))]
    [(">") (> (car expr) (car (cddr expr)))]
    [("<") (< (car expr) (car (cddr expr)))]))



