#lang racket
(require "variables.rkt")
(require "shunting-yard.rkt")
(require "postfix-evaluator.rkt")
(provide assignmentStatement)

; takes a tokenized assignment statement e.g. "a" #\= 10 #\+ 5
; Sets the value of a variable on the lefthand side
; to the result of the expression on the right hand side of the equal sign 
(define (assignmentStatement expr)
  (setVarValue variables (car expr) (evalPostFix (shunting-yard (cddr expr)))))

