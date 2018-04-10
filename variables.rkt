#lang racket
(require compatibility/mlist)
(provide checkVarExists)
(provide variables)
(provide defineVar)
(provide getVarValue)
(provide setVarValue)
(provide clearVariables)
(provide replaceVars)
(provide replace)


; List of user-defined variables. Format is (string, string, integer) => (name, type, value)
(define variables (mlist (mlist #\I "integer" 22) (mlist #\J "integer" 0)))


; Defines a new variable in the variables list
; name = string
; type = string
; value = integer/float/bool
(define (defineVar name type value)
  (mappend! variables (mlist (mlist (car (string->list name)) type value)));
)


; Returns the value of a variable
; input = mlist (variables)
; name = string
(define getVarValue
    (lambda (input name)
        (if (null? input)

            ; The variable does not exist
            "Variable not defined"

            ; The variable may exist, keep looking
            (if (equal? (mcar (mcar input)) name)
                
                ; We found the variable, return its value
                (mlist-ref (mcar input) 2)

                ; Haven't found the variable, keep looking
                (getVarValue (mcdr input) name)
            )
        )
    )
)

; Returns #t or #f when looking for a variable in the stack.
; input = mlist (variables)
; name = string
(define (checkVarExists input name)
  (if (null? input)
      #f
      (if (equal? (mcar (mcar input)) name)
          name
          (checkVarExists (mcdr input) name))))
      

; Sets the value of a variable
; input = mlist (variables)
; name = string
; value = integer/float/bool
(define setVarValue
    (lambda (input name value)
        (if (null? input)

            ; The variable does not exist
            "Variable not defined"

            ; The variable may exist, keep looking
            (if (equal? (mcar (mcar input)) name)
                
                ; We found the variable, set its value
                (set-mcar! (mcdr (mcdr (mcar input))) value)
                
                ; Haven't found the variable, keep looking
                (setVarValue (mcdr input) name value)
            )
        )
    )
)

; Replaces the variables in a given expression with their literal values
; expr = A list with mixed types
; eg) If a=10, b=4 and we get an input of     '(3 #\+ #\a #\* #\b)
; the return will be    '(3 #\+ 10 #\* 4)
(define (replaceVars expr)
  (if (null? expr)
      '()
      (cons (replace (car expr)) (replaceVars (cdr expr)))
  )
)

(define replace
  (lambda (n)
       (if (checkVarExists variables n)

          ; Element is a variable, replace it
          (getVarValue variables n)
           
          ; Element is not a variable, do not change it
          n
       )
   )
)

; Clears all variables
(define (clearVariables)
  (set! variables (mlist (mlist #\I "integer" 0) (mlist #\J "integer" 0)))
)