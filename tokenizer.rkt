#lang racket
(require "helpers.rkt")
(require "variables.rkt")
(provide tokenize)

(define (tokenize expr)
  (let ((parts (string-split expr)))
    (cond
      [(null? parts) '()]
      [(command? (first parts)) (%tokenize-command expr)]
      [(io? (first parts)) parts]
      [(selection? (first parts)) (%tokenize-selection parts)]
      [(iterative? (first parts)) (%tokenize-iterative parts)]
      [(booleaneval? expr) (%tokenize-boolean-eval expr)]
      [else (%tokenize (string->list expr))])))

(define (%tokenize-command expr)
  (let ((command (first (string-split expr))))
    (case command
      (("#definefunc") (%tokenize-definefunc expr))
      (else (string-split expr)))))

(define (%tokenize-definefunc expr)
  (let ((lines (string-split expr "\r")))
    (append '() (map (lambda (line) (string-split line)) lines))))

(define (%tokenize-boolean-eval parts)
  (let ((e (string-split parts #rx"==|<>|>=|<=|>|<")))
    (list (first e) (first (booleaneval? parts)) (second e))))
    
(define (%tokenize-selection parts) "tokenize seletion")
(define (%tokenize-iterative parts) "tokenize iterative")

(define (%tokenize parts)
    (cond ((null? parts) '())
          ((or (operator? (car parts))
               (bracket? (car parts)))
           (cons (car parts)
                 (%tokenize (cdr parts))))
          ((char-numeric? (car parts))
           (cons (string->number (list->string (parse-number parts)))
                 (%tokenize (list-tail parts (length (parse-number parts))))))
          ((variable? (car parts)))
          (else (%tokenize (cdr parts)))))

(define (parse-number eqn)
  (cond ((null? eqn)
         '())
        ((char-numeric? (car eqn))
         (cons (car eqn) (parse-number (cdr eqn))))
        ((eq? (car eqn) #\.)
         (cons (car eqn) (parse-number (cdr eqn))))
        (else '())))

