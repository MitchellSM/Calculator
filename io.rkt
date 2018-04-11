#lang racket
(provide handle-io-output)
(provide handle-io-input)
(require "variables.rkt")

;;;Input and Output Handling


; receives '("output" "a")
(define (handle-io-output expr) (number? (getVarValue variables (car (string->list (car expr)))))
                                    (string-append "The result is " (number->string (getVarValue variables (car (string->list (cadr expr)))))))

; receives '("input" "a")
(define (handle-io-input expr) (setVarValue variables (string-ref (cadr expr) 0) (read)))
