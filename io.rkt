#lang racket
(require "variables.rkt")
;;;Input and Output Handling

(define output
  (lambda (var)
    (printf "~a" getVarValue(variables var))))

(define input
  (lambda (var)
    "Do something here"))