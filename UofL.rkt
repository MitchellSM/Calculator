#lang scheme

(define (eval expr)
  (let ([e (string-split expr)])
    (case (car e)
      (("#exit") (exit))
      (("#definevari") (define-variable e))
      (("#definefunc") (define-function e))
      (("#clear") (clear-variables))
      (else (eval-basic-expr e)))))

(define (define-variable e) "DEFINE A FUCKING VARIABLE")
(define (define-function e) "DEFINE A FUCKING FUNCTION")
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
    (write (eval expr))
    (newline)
    (UofL)))