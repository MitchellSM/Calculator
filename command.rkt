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

(define (handle-exit tokens) "define the handle-exit handler here")
(define (handle-clear tokens) "define the handle-clear handler here")

(define (handle-define-function tokens) "mitch work here")

