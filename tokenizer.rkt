#lang racket
(require "helpers.rkt")
;;(require "commandprocessor.rkt")
(provide tokenize)

(define (tokenize expr)
  (if (not (command? expr))
      (%tokenize (string->list expr))
      "GOT COMMAND"))

(define (%tokenize parts)
    (cond ((null? parts)
           '())
          ((or (operator? (car parts))
               (bracket? (car parts)))
           (cons (car parts) (%tokenize (cdr parts))))
          ((char-numeric? (car parts))
           (cons (string->number (list->string (parse-number parts)))
                 (%tokenize (list-tail parts (length (parse-number parts))))))
          (else (%tokenize (cdr parts)))))

(define (parse-number eqn)
  (cond ((null? eqn)
         '())
        ((char-numeric? (car eqn))
         (cons (car eqn) (parse-number (cdr eqn))))
        (else '())))
           