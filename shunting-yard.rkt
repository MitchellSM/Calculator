#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

; Gets the associativity of a given operator
; operator = An operator, represented as a char
(define (get-associativity operator)

  ; Check the associativity of the operator
  (if (member operator '(#\+ #\- #\* #\/ #\%))
      'left
      'right
  )
)


; Determines the precedence of a given operator
; operator = An operator, represented as a char
(define (get-precedence operator)
  (case operator
    ((#\= #\< #\>) 1)
    ((#\+ #\-)     2)
    ((#\* #\/ #\%) 3)
    ((#\^ #\!)     4)
    (else          0)
  )
)


; Special operator conditions
(define (operator-actions stmt stack)
  (let* (
         ; Get the token's precedence
         (token-precedence (get-precedence (car stmt)))

         ; Get the token's associativity
         (token-assoc (get-associativity (car stmt)))

         ; Get the stack operator
         (stack-oper
            (if (not (null? stack))
                (car stack)
                '()
            )
         )

         ; Get the stack operator's precedence
         (stack-precedence
            (if (not (null? stack-oper))
                (get-precedence stack-oper)
                0
            )
         ))

    ; Check the following conditions and pop operators from stack
    ; Is the op at the top of the stack with greater precedence?
    ; Is the op at the top of the stack equal precedence and left associative?
    ; Is the op at the top of the stack not a left bracket?
    (cond ((or (and (eq? token-assoc 'left)
                    (<= token-precedence stack-precedence))
               (and (eq? token-assoc 'right)
                    (< token-precedence stack-precedence)))
           (cons stack-oper (%shunting-yard stmt (cdr stack))))
          (else (%shunting-yard (cdr stmt) (cons (car stmt) stack))))
  )
)


; Check if statement is null
(define (stack-operations stack)
  (cond ((and (not (null? stack))
              (eq? (car stack) #\())
         (display "Incorrect format"))
        ((null? stack) '())
        (else (cons (car stack) (%shunting-yard '() (cdr stack))))
  )
)


; The shunting-yard algorithm takes an infix expression and converts it
; to a postfix expression
(define (%shunting-yard stmt stack)
  (cond ((null? stmt)
         (stack-operations stack))

        ; Numbers always go onto the stack
        ((number? (car stmt))
         (cons (car stmt) (%shunting-yard (cdr stmt) stack)))

        ; Operators have special conditions
        ((operator? (car stmt))
         (operator-actions stmt stack))

        ; Left brackets always go onto the stack
        ((eq? (car stmt) #\()
         (%shunting-yard (cdr stmt) (cons (car stmt) stack)))

        ; Right brackets require us to pop until we find a matching left bracket
        ((eq? (car stmt) #\))     
         (if (eq? #\( (car stack))
             (%shunting-yard (cdr stmt) (cdr stack))
             (cons (car stack) (%shunting-yard stmt (cdr stack)))))
  )
)

(define (shunting-yard stmt)
  (%shunting-yard stmt '()))
