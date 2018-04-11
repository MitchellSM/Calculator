#lang racket
(require "variables.rkt")
(provide handle-define-vari)
(provide handle-exit)
(provide handle-clear)
(provide handle-define-function)


(define (handle-define-vari tokens)
  (defineVar (list-ref tokens 1) (list-ref tokens 2) 0)
  "Variable added"
)

(define (handle-exit tokens) (exit))
(define (handle-clear tokens) (clearVariables))

(define (handle-define-function tokens) "mitch work here")

