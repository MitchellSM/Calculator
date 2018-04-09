#lang racket
(provide (all-defined-out))
      
;;; function which returns #f if symbol is an operator.
(define (operator? symbol)
  (member symbol '(#\+ #\- #\* #\/ #\^ #\!)))

;;; function which returns #f if symbol is a bracket.
(define (bracket? symbol)
  (member symbol '(#\( #\))))

;;; This function returns #t if symbol is a command.
(define (command? symbol)
  (case symbol
    (("#exit") #t)
    (("#definevari") #t)
    (("#definefunc") #t)
    (("#clear") #t)
    (else #f)))

;;; This function returns #t if symbol is an io operator.
(define (io? symbol)
  (case symbol
    (("input") #t)
    (("output") #t)
    (else #f)))

(define (selection? symbol)
  (case symbol
    (("if") #t)
    (("then") #t)
    (("elseif") #t)
    (("endif") #t)
    (else #f)))

(define (iterative? symbol)
  (case symbol
    (("for") #t)
    (("to") #t)
    (("stepsize") #t)
    (("endfor") #t)
    (else #f)))

(define (variable? symbol)
  (member symbol '(a)))