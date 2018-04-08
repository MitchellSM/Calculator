#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

; Determines the associativity of a given operator
; operator = An operator
(define (associativity-of operator)
  (if (member operator '(#\+ #\- #\* #\/ #\%))
      'left
      'right
  )
)

; Determines the precedence of a given operator
; operator = An operator
(define (precedence-of operator)
  (case operator
    ((#\= #\< #\>) 1)
    ((#\+ #\-)     2)
    ((#\* #\/ #\%) 3)
    ((#\^ #\!)     4)
    (else          0)
  )
)


; Returns the last element of a list
; lst = A list
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))
   )
)


; Checks if a given character is numerical
(define (digit-char? char)
  (> 10
     (- (char->integer char) (char->integer #\0))
     (- 1)
  )
)


; ???
;(define digit-char? char-numeric?)


; Actions to take if the token in the stmt is an operator
(define (operator-actions stmt stack)
  (let* ((token-precedence (precedence-of (car stmt)))
         (token-assoc (associativity-of (car stmt)))
         (stack-oper
            (if (not (null? stack))
                (car stack)
                '()
            )
         )
         (stack-precedence
            (if (not (null? stack-oper))
                (precedence-of stack-oper)
                0
            )
         )
         )
    (cond ((or (and (eq? token-assoc 'left)
                    (<= token-precedence stack-precedence))
               (and (eq? token-assoc 'right)
                    (< token-precedence stack-precedence)))
           (cons stack-oper (%shunting-yard stmt (cdr stack))))
          (else (%shunting-yard (cdr stmt) (cons (car stmt) stack))))
  )
)


; Actions to take if (null? stmt)
(define (stack-operations stack)
  ;; If a left-parenthesis is found on the stack,
  ;; it means there was no right-parenthesis to match it
  ;; and thus the statement has unbalanced parentheses.
  (cond ((and (not (null? stack))
              (eq? (car stack) #\())
         (display "Unbalanced parenthesis"))
        ((null? stack) '())
        (else (cons (car stack) (%shunting-yard '() (cdr stack))))))


;; Implementation of Dijkstra's Shunting-yard Algorithm
; "Converts infix-notation mathematical equations into
;postfix-notation mathematical equations, using an
;implementation of Dijkstra's Shunting-yard Algorithm."
(define (%shunting-yard stmt stack)
  (cond ((null? stmt)
         (stack-operations stack))
        ((number? (car stmt))
         (cons (car stmt) (%shunting-yard (cdr stmt) stack)))
        ((operator? (car stmt))
         (operator-actions stmt stack))
        ((eq? (car stmt) #\()
         (%shunting-yard (cdr stmt) (cons (car stmt) stack)))
        ((eq? (car stmt) #\))     
         (if (eq? #\( (car stack))
             (%shunting-yard (cdr stmt) (cdr stack))
             (cons (car stack) (%shunting-yard stmt (cdr stack)))))
  )
)

(define (shunting-yard stmt)
  (%shunting-yard stmt '()))
