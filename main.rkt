#lang racket
(require "parser.rkt")

;; input multi line mode.
(define multimode #f)
(define (modeswitch)
  (if multimode
      (set! multimode #f)
      (set! multimode #t)))

(define (togglemode expr)
  (if (not (null? (string-split expr)))
      (if (member (car (string-split expr)) '("#definefunc" "if" "endif" "for" "endfor"))
          (modeswitch)
          expr) expr))

;; This is the internal version of the REPL, called with ease
;; by the UofL function
(define (%UofL command)
  (display "UofL>")
  (let ((expr (read-line)))
    ((togglemode expr)
     (if multimode
         (%UofL (string-append command (string-append expr "\r")))
         ((write (parse (string-append command expr)))
         (newline)
         (%UofL ""))))))

;; This is the main entry point for the entire application.
(define (UofL)
  (%UofL ""))
