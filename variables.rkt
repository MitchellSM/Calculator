#lang racket
(require compatibility/mlist)
(provide variables)
(provide defineVar)
(provide getVarValue)
(provide setVarValue)
(provide clearVariables)


; List of user-defined variables. Format is (string, string, integer) => (name, type, value)
(define variables (mlist (mlist "I" "integer" 22) (mlist "J" "integer" 0)))


; Defines a new variable in the variables list
; name = string
; type = string
; value = integer/float/bool
(define (defineVar name type value)
  (mappend! variables (mlist (mlist name type value)));
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


; Clears all variables
(define (clearVariables)
  (set! variables (mlist (mlist "I" "integer" 0) (mlist "J" "integer" 0)))
)