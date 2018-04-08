#lang racket
(require "helpers.rkt")
(require "tokenizer.rkt")
(provide handle)

;;; This function parses input
;;; and kicks off handling.
;;;
;;; Input: string expression.
(define (parse expr)
  (let ([tokens (tokenize expr)])
    (handle tokens)))

(define (handle tokens)
  (let ([f (first (tokens)])
    (case f
      [("#definevari") (handlecommand expr)]
      [("#definefunc") (handlecommand expr)]
      [("#exit") (handlecommand expr)]
      [("#clear") (handlecommand expr)]
      [("input") (handleinput expr)]
      [("output") (handleoutput expr)]
      [("if") (handleselection expr)]
      [("for") (handleiterative expr)]
      [else expr])))

(define (handlecommand expr) "got command")
(define (handleinput expr) "got input")
(define (handleoutput expr) "got output")
(define (handleselection expr) "got selection")
(define (handleiterative expr) "got iterative")