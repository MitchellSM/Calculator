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
    (("main") #t)
    (else #f)))

;;; This function returns #t if symbol is an io operator.
(define (io? symbol)
  (case symbol
    (("input") #t)
    (("output") #t)
    (else #f)))

;;; This function returns #t if symbol is a selection symbol.
(define (selection? symbol)
  (case symbol
    (("if") #t)
    (("then") #t)
    (("elseif") #t)
    (("endif") #t)
    (else #f)))

;;; This function return #t is symbol is an iterative symbol.
(define (iterative? symbol)
  (case symbol
    (("for") #t)
    (("to") #t)
    (("stepsize") #t)
    (("endfor") #t)
    (else #f)))

(define (selectioncheck? expr)
  (regexp-match #rx"if|then|elseif|endif" expr))

(define (iterativecheck? expr)
  (regexp-match #rx"for|to|stepsize|endfor" expr))

;;; This function returns #t if symbol is a boolean evaluation symbol.
(define (booleaneval? expr)
  (regexp-match #rx"==|<>|>=|<=|>|<" expr))

;;; This function returns #t if expression is assignment.
(define (assignment? expr)
  (regexp-match #rx"=" expr))