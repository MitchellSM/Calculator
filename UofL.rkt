#lang scheme
(require "tokenizer.rkt")

(define (UofL)
  (display "UofL>")
  (let ((expr (read-line)))
    (write (tokenize expr))
    (newline)
    (UofL)))