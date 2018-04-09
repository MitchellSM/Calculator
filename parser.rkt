#lang racket
(require "helpers.rkt")
(require "tokenizer.rkt")
(require "shunting-yard.rkt")
(require "postfix-evaluator.rkt")
(require "command.rkt")
(require "functions.rkt")
(require "io.rkt")
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
    ; special handling for functions.
    (if (pair? f) (handle-define-function tokens)
        (case f
          [("#definevari") (handle-define-vari tokens)]
          [("#exit") (handle-exit tokens)]
          [("#clear") (handle-clear tokens)]
          [("input") (handle-io-input tokens)]
          [("output") (handle-io-output tokens)]
          [("if") (handleselection tokens)]
          [("for") (handleiterative tokens)]
          [("main") (handle-main)]
          [else (evalPostFix(shunting-yard tokens))]))))

(define (handlecommand expr) expr)
(define (handleselection expr) "got selection")
(define (handleiterative expr) "got iterative")