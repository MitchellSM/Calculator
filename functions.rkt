#lang racket
;;;FUNCTION HANDLING
(require compatibility/mlist)
(require "helpers.rkt")
(provide handle-main)

(define (handle-main) "IN MAIN")

(define functions (mlist (mlist "myfunc" '("a" "b") '("a" #\= "b" #\+ #\2)) (mlist "myfunc2" '("c" "d") '("c" #\= "d" #\* #\3))))

;;; Definition of a function
;;; name = String -> "myfunc"
;;; parameter_list = list -> '("var1" ... "varN")
;;; body_statements = list -> '("stmt1" ... "stmtN")
(define (defineFunc name args body)
  (mappend! functions (mlist (mlist name args body))))

;;; Gets the arugment list of a specific function
(define getArgs
  (lambda (funcs name)
    (if (null? funcs)
        "No user-defined functions found"
        (if (equal? (mcar (mcar funcs)) name)
                (mlist-ref (mcar funcs) 1)
                (getArgs (mcdr funcs) name)))))

;;; Gets the body of a specific function
(define getBody
  (lambda (funcs name)
    (if (null? funcs)
        "No user-defined functions found"
        (if (equal? (mcar (mcar funcs)) name)
            (mlist-ref (mcar funcs) 2)
            (getBody (mcdr funcs) name)))))

(define functionParse
  (lambda (str)
    (let* ([func (string-split str)]
           ;remove commands
           [func (cdr func)] [func (reverse (cdr(reverse func)))]
           ;Parse out name
           [name (car func)] [func (cdr func)]
           
;;;THESE ARE NOT FINISHED...
;;;NEED TO PROPERLY PARSE OUT ARGS
           
          ;Parse out parameters
           [args (take func 2)]
          ;parse out body
           [body (take-right func 2)])
      
        ;build function
      (defineFunc name args body))))
  
;;; TESTS
;(defineFunc "myfunc3" '(#\e #\f) '(#\e #\= #\f #\* #\3 #\g))

;(getArgs functions "myfunc2")
;(getBody functions "myfunc2")

;(getArgs functions "myfunc")
;(getBody functions "myfunc")

;(getArgs functions "myfunc3")
;(getBody functions "myfunc3")

;(functionParse "#definefunc myfunc4 a b a=b+3 b=2+a #definefunc")
;(getArgs functions "myfunc4")
;(getBody functions "myfunc4")
