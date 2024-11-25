; Expressions --------------------------------------------------------------------
(+ 1 2 3)  ; 6
(* 2 3 4)  ; 24

; Variables --------------------------------------------------------------------
(define x 10)
(define y 20)
(+ x y)  ; x + y = 30

; Function --------------------------------------------------------------------
(define (add x y)
    (+ x y))

(add 10 20)  ; 30

; Conditionals --------------------------------------------------------------------
(define x 10)
(define y 20)
;(if (> x y)
 ;   (display "x is greater")                ;  If Clause
  ;  (display "y is greater or equal"))      ;  Else Clause

; Conditionals: Multiple Clauses
;(cond 
 ;   ((> x y) "x is greater")      ; If Clause
  ;  ((< x y) "y is greater")      ; Else If Clause
   ; (else "x and y are equal"))   ; Else Clause / Default Case

; Lambda Expression --------------------------------------------------------------------
;((lambda (x y) (+ x y)) 3 4)  ; 7
; Also called unamed procedure
;((lambda (x y) (+ x y)) 3 4) ; 7


; First Class Function --------------------------------------------------------------------
; Map function 
(define (map f lst)
    if (null? lst)
        '()
        (cons (f (car lst)) (map f (cdr lst))))


; Checking Equality in Scheme --------------------------------------------------------------------
; = use to check equality of numbers
(= 3 3) ; #t
(= 3 4) ; #f
(define x 10)
(define y 10)
(= x y) ; #t
(define z 20)
(= x z) ; #f
(= 6 (+ 2 4)) ; #t


; eq? 
; eq? is used to check if two objects are the same
;(eq? 'a 'a) ; #t
;(eq? 'a 'b) ; #f

(define x 10)
(define y 10)
;(eq? x y) ; #t

(define lst1 '(1 2 3))
(define lst2 '(1 2 3))
;(eq? lst1 lst2) ; #f because they are different objects in memory

;equal?
; equal? used for deep comparison checking if they have the same structure and content 
;(equal? 'a 'a) ; #t
;(equal? 'a 'b) ; #f
; compare a list
(define lst1 '(1 2 3))
(define lst2 '(1 2 3))
;(equal? lst1 lst2) ; #t
; compare a list with a different order
(define lst3 '(3 2 1))
;(equal? lst1 lst3) ; #f
; compare a list with same content but different structure
(define lst4 '(1 (2 3)))
(define lst5 '(1 2 3))
;(equal? lst4 lst5) ; #f


; List -------------------------------------------------------------------------------------------------------------------

;List 
(list 10 20 30)    ; (10 20 30)

; Quote
'(10 20 30)        ; (10 20 30)
(quote (10 20 30)) ; (10 20 30)

; List and Quote
(define x 20)
(list 10 x 30)          ; (10 20 30)
'(10 x 30)              ; (10 x 30)

; List are Heterogeneous
(list 10 "hello" 3.14) ; (10 "hello" 3.14)
; Difference of list (multiple) and quote (1 arg)
(+ 1 2) ;3
'(+ 1 2)  ;(+ 1 2)
(quote(+ 1 2)) ;(+ 1 2)
(list + 1 2) ;(#<procedure+> 1 2)
(list (+ 1 2)) ;(3)

(define x 10)
(define y 20)
(define z 30)
(define double 
    (lambda (x) (* 2 x)))

(define (double x) (* 2 x))
(define lst2 (list    'a 2 "Hello" 'world / #f + double 10 x 'y 'a #\H '(1 2 3) (+ 3 1)  ))      ; Evaluates each piece the expression
(define lst3 (quote ( 'a 2 "Hello" 'world / #f + double 10 x 'y 'a #\H '(1 2 3) (+ 3 1) )))      ; Does not valuate the expression
;(display "Using list: ")
;(display lst1)
(newline)
;(display "Using quote: ")
;(display lst2)

; Define a dynamic list (constructed using `list`)
(define dynamic-list
    (list 10
        'x                ; Symbol explicitly quoted
        (+ 2 3)           ; Expression that evaluates to 5
        '(a b c)          ; Quoted list, treated as literal
        "hello"))         ; String
;(display dynamic-list)    ; (10  x    5     (a b c) "hello")

; Define a static list (constructed using `quote`)
(define static-list
    '(10                  ; Number, literal
    'x                   ; Symbol, not evaluated
    (+ 2 3)             ; Expression, not evaluated
    '(a b c)             ; List, literal
    "hello"))           ; String
;(display static-list)   ; (10 x (+ 2 3) (quote (a b c)) hello)

(define x 3)
(define dynamic-list   (list 10  x (+ 2 3) '(a b c) "hello"))
;(display dynamic-list)    ; (10  3    5     (a b c) "hello")

(define static-list      '(10 x (+ 2 3) '(a b c) "hello"))
;(display static-list)   ; (10 x (+ 2 3) (quote(a b c)) hello)

;(display lst2)
;(display lst3)

; List Operations

; car
(car '(1 2 3)) ; 1 
; cdr
(cdr '(1 2 3)) ; (2 3) 

; Getting sum of a list using car and cdr



(car (cdr '(1 2 3))) ; 2 - gets the second element of the list
(cdr (cdr (cdr '(1 2 3)))) ; () - gets the tail of the tail of the tail of the list
(cdr '((1 2 3))) ; () - gets the tail of the list 
(cadr '(1 2 3)) ; 2 - gets the second element of the list
(cddr '(1 2 3)) ; (3) - gets the tail of the tail of the list

(define lst4 '((1 2) (3 4 (5 6)) (7 8) ))
(car lst4) ; (1 2)
(cdr lst4) ; ((3 4 (5 6)) (7 8))
(car (car lst4)) ; 1
(cdr (car lst4)) ; (2)
(car (cdr lst4)) ; (3 4 (5 6))
(cdr (cdr lst4)) ; ((7 8))
(define oplst (list + - * /))
((car oplst) 10 20) ; 30

(define nested-list '(   (1 2) (3 4) (5 6) (7 8 9)  ))

; Extract the first sublist
(display (car nested-list))  ;  (1 2)
(newline)

; Extract the second element of the first sublist
(display (cadr (car nested-list)))  ; 2
(newline)

; Extract the third sublist
(display (caddr nested-list))  ; (5 6)
(newline)

; Extract the first element of the fourth sublist
(display (car (cadddr nested-list)))  ; 7
(newline)

; Extract the second element of the second sublist
(display (cadr (cadr nested-list)))  ; 4
(newline)


(define nested-list '(   (1 2) (3 4) (5 6) (7 8 9)  ))

(car nested-list)          ; (1 2)
(cadr (car nested-list))   ; 2
(caddr nested-list)        ; (5 6)
(car (cadddr nested-list)) ; 7
(cadr (cadr nested-list))  ; 4


; append : append two lists
(append '(1 2 3) '(4 5 6)) ; (1 2 3 4 5 6)

; cons 
(cons 1 '(2 3 4))               ; (1 2 3 4)
(cons 1 (cons 2 (cons 3 '())))  ; (1 2 3)
(cons '(1 2 3) 1)               ; ((1 2 3) . 1)






; Macros 
(define-syntax 
    3-state
    (syntax-rules ()
        ((3-state
            value positive-body zero-body negative-body)
            (cond
                ((zero? value) zero-body)
                ((positive? value) positive-body)           
                (else negative-body) ))))


; Procedure Examples -----------------------------------------------------------------------------------------------
; duplicating every el element in a list
