#lang racket
(require "helpers.rkt")
(require "tokenizer.rkt")
(require "shunting-yard.rkt")
(require "postfix-evaluator.rkt")
(require "command.rkt")
(provide parse)

;;; This function parses input
;;; and kicks off handling.
;;;
;;; Input: string expression.
(define (parse expr)
  (let ([tokens (tokenize expr)])
    (handle tokens)))

;;; This function handles the tokens
;;; from the tokenizer.
;;;
;;; Input: list of tokens.
(define (handle tokens)
  (let ([f (first tokens)])
    (if (pair? f) (handle-define-function tokens)
        (case f
          [("#definevari") (handlecommand tokens)]
          [("#definefunc") (handlecommand tokens)]
          [("#exit") (handlecommand tokens)]
          [("#clear") (handlecommand tokens)]
          [("input") (handleinput tokens)]
          [("output") (handleoutput tokens)]
          [("if") (handleselection tokens)]
          [("for") (handleiterative tokens)]
          [else (evalPostFix(shunting-yard tokens))]))))

(define (handlecommand expr) expr)
(define (handleinput expr) "got input")
(define (handleoutput expr) "got output")
(define (handleselection expr) "got selection")
(define (handleiterative expr) "got iterative")