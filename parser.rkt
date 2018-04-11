#lang racket
(require "helpers.rkt")
(require "tokenizer.rkt")
(require "shunting-yard.rkt")
(require "postfix-evaluator.rkt")
(require "command.rkt")
(require "functions.rkt")
(require "io.rkt")
(require "variables.rkt")
(provide parse)

;;; This function parses input
;;; and kicks off handling.
;;;
;;; Input: string expression.
(define (parse expr)
  (cond ((booleaneval? expr) (handle-boolean-eval (tokenize expr)))
        ((assignment? expr) (handle-assignment (tokenize expr)))
        (else (let ([tokens (tokenize expr)])
         (handle tokens)))))
  
;;; This function handles the tokens
;;; from the tokenizer.
;;;
;;; Input: list of tokens.
(define (handle tokens)
  (let ([f (first tokens)])
    ; special handling for functions.
    (if (pair? f) (handle-define-function tokens)
        (case f
          ; tokens = ("#definevari" "varname" "vartype")
          [("#definevari") (handle-define-vari tokens)]
          ; tokens = ("#exit")
          [("#exit") (handle-exit tokens)]
          ; tokens = ("#clear")
          [("#clear") (handle-clear tokens)]
          ; tokens = ("input" "varname")
          [("input") (handle-io-input tokens)]
          ; tokens = ("output" "varname")
          [("output") (handle-io-output tokens)]
          [("if") (handleselection tokens)]
          [("for") (handleiterative tokens)]
          ; tokens = ("main")
          [("main") (handle-main)]
          ; tokens = list of single characters representing equation
          [else (evalPostFix(shunting-yard (replaceVars tokens)))]))))

; gets ("expr" "operator" "expr")
(define (handle-boolean-eval expr)
  (let ([l (parse (first expr))]
        [o (second expr)]
        [r (parse (third expr))])
    (case o
      (("==") (equal? l r))
      (("<>") (not (equal? l r)))
      ((">=") (>= l r))
      (("<=") (<= l r))
      ((">") (> l r))
      (("<") (< l r)))))

; gets '("1" "=" "2")
(define (handle-assignment expr)
  (setVarValue variables (car (string->list (car expr)))  (parse (caddr expr))))

(define (handleselection expr) expr)

(define (handleiterative expr) expr)