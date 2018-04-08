#lang scheme
(require "tokenizer.rkt")

(define stack '((a b)))

(define (eval expr)
  (let ([e (string-split expr)])
    (case (car e)
      (("#exit") (exit))
      (("#definevari") (define-variable e))
      (("#definefunc") (define-function e))
      (("#clear") (clear-variables))
      (else (eval-basic-expr e)))))

(define (define-variable e)
   (set! stack (append stack (list (list (cadr e) (caddr e)))))
)

(define (define-function e) stack)

(define (clear-variables) "CLEAR THE FUCKING VARIABLES")

(define (eval-basic-expr e)
  (let ([l (string->number (first e))]
        [o (second e)]
        [r (string->number (third e))])
    (case o
      (("+") (+ l r))
      (("-") (- l r))
      (("*") (* l r))
      (("/") (/ l r))
      (("^") (expt l r))
      (("==") (if (= l r) "true" "false"))
      (("<>") (if (not (= l r)) "true" "false"))
      ((">=") (if (>= l r) "true" "false"))
      (("<=") (if (<= l r) "true" "false"))
      ((">") (if (> l r) "true" "false"))
      (("<") (if (< l r) "true" "false"))
      (else "CANT EVALUATE"))
    ))

(define (UofL)
  (display "UofL>")
  (let ((expr (read-line)))
    (write (eval (tokenize expr)))
    (newline)
    (UofL)))