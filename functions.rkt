#lang racket
;;;FUNCTION HANDLING
(require compatibility/mlist)
(require "helpers.rkt")
(provide handle-main)
(provide defineFunc)
(provide function?)
(provide execute)
(provide functions)
    
(define (handle-main tokens)
  (begin
    (execute "testFunc1" (1 2))
    (execute "testFunc2" ((+1 2) 3))))

(define functions (mlist (mlist "testFunc1" '("a" "b") '(("a=b+2")("b=a*2"))) (mlist "testFunc2" '("c" "d") '(("c=d*3")("d=c-5")))))

(define (defineFunc name args body)
  (mappend! functions (mlist (mlist name args body))))

; Gets the arugment list of a specific function
(define getArgs
  (lambda (funcs name)
    (if (null? funcs)
        "No user-defined functions found"
        (if (equal? (mcar (mcar funcs)) name)
                (mlist-ref (mcar funcs) 1)
                (getArgs (mcdr funcs) name)))))

; Gets the body of a specific function
(define getBody
  (lambda (funcs name)
    (if (null? funcs)
        "No user-defined functions found"
        (if (equal? (mcar (mcar funcs)) name)
            (mlist-ref (mcar funcs) 2)
            (getBody (mcdr funcs) name)))))

; Sets the body of a function after variable replacement
(define setBody
  (lambda (funcs name body)
    (if (equal? (mcar (mcar funcs)) name)
        (set-mcar! (mcdr(mcdr(mcar funcs))) body)
        (setBody (mcdr funcs) name body))))
        

; Replaces all values and replaces old body statements
(define execute
  (lambda (name para)
    (let ([body (getBody functions name)]
          [args (getArgs functions name)])
        (setBody functions name (exe body para args)))))

; Chains together each body statement after they have been replaced
(define exe
  (lambda (body para args)
    (if (null? body)
        '()
        (cons (replacevars (string->list(caar body)) para args) (exe (cdr body) para args)))))

; Replaces each variable with the given parameter
(define (replacevars expr para args)
  (if (null? expr)
      '()
        (cons (replace (car expr) para args) (replacevars (cdr expr) para args))))

; Verifies replacement of given variables
(define replace
  (lambda (n para args)
    (if (null? args)
        n
        (if (eqv? n (car(string->list (car args))))
            (car para)
            (replace n (cdr para) (cdr args))))))

; True if function is defined on stack, false otherwise
(define function?
  (lambda (funcs name)
    (if (null? funcs)
        #f
        (if (equal? (mcar (mcar funcs)) name)
            name
            (function? (mcdr funcs) name)))))
