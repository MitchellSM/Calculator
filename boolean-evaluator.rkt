#lang racket
(require "parser.rkt")
(provide evalBoolean)

; evaluates a boolean expresion of the form '("10 + 1" ">" "100 - 99")
; returns true or false
(define (evalBoolean expr)
  (%evalBoolean (parse (car expr)) (cadr expr) (parse (caddr expr)))) 

(define (%evalBoolean l opp r)
  (case opp
    [("==") (eq? l r)]
    [("<>") (not (eq? l r))]
    [(">=") (>= l r)]
    [("<=") (<= l r)]
    [(">") (> l r)]
    [("<") (< l r)]))

