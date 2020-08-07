#lang racket
(require "parser.rkt")
(require "variables.rkt")
(provide assignmentStatement)

; takes a tokenized assignment statement e.g. '("a" "10 + 5")
; Sets the value of a variable on the lefthand side of the equal sign 
; to the result of the expression on the right hand side 
(define (assignmentStatement expr)
  (setVarValue variables (car expr)  (parse (cadr expr))))

