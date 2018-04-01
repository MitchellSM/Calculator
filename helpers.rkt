#lang racket
(provide operator?)
(provide bracket?)
(provide command?)

;;; function which returns #f if symbol is an operator.
(define (operator? symbol)
  (member symbol '(#\+ #\- #\* #\/ #\% #\^ #\! #\= #\< #\>)))

;;; function which returns #f if symbol is a bracket.
(define (bracket? symbol)
  (member symbol '(#\( #\))))

(define (command? symbol)
  (case symbol
    (("#exit") #t)
    (("#definevari") #t)
    (("#definefunc") #t)
    (("#clear") #t)
    (else #f)))