#lang racket
(require "variables.rkt")
(require "functions.rkt")
(provide handle-define-vari)
(provide handle-define-function)
(provide handle-exit)
(provide handle-clear)

(define (handle-define-vari tokens)
  (defineVar (list-ref tokens 1) (list-ref tokens 2) 0)
  "Variable added"
)

(define (handle-define-function tokens)
  (defineFunc
    (list-ref(car tokens)1)
    (take-right(car tokens)2)
    (cdr(reverse(cdr(reverse tokens)))))
  "Function added")

(define (handle-exit tokens) (exit))
(define (handle-clear tokens) (clearVariables))





