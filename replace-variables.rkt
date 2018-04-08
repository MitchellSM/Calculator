#lang racket
(require "variables.rkt")
(provide replaceVars)


; Replaces the variables in a given expression with their literal values
; expr = A list with mixed types
; eg) If myvar=10, b=4 and we get an input of     '(3 #\+ "myvar" #\* "b")
; the return will be    '(3 #\+ 10 #\* 4)
(define (replaceVars expr)
  (if (null? expr)
      '()
      (cons (replace (car expr)) (replaceVars (cdr expr)))
  )
)

(define replace
  (lambda (n)
       (if (string? n)

          ; Element is a variable, replace it
          (getVarValue variables n)
           
          ; Element is not a variable, do not change it
          n
       )
   )
)
