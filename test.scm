; Expressions
(+ 1 2 3)  ; 6
(* 2 3 4)  ; 24

; Variables
(define x 10)
(define y 20)
(+ x y)  ; x + y = 30

; Function
(define (add x y)
    (+ x y))

(add 10 20)  ; 30

; Conditionals
(define x 10)
(define y 20)
(if (> x y)
    (display "x is greater")                ;  If Clause
    (display "y is greater or equal"))      ;  Else Clause

; Conditionals: Multiple Clauses
(cond 
    ((> x y) "x is greater")      ; If Clause
    ((< x y) "y is greater")      ; Else If Clause
    (else "x and y are equal"))   ; Else Clause / Default Case


; Recursion
(define (factorial_tr))

; Lambda Expression
; Also called unamed procedure
((lambda (x y) (+ x y)) 3 4) ; 7


; First Class Function
; Map function 
(define (map f lst)
    if (null? lst)
        '()
        (cons (f (car lst)) (map f (cdr lst))))


; Checking Equality in Scheme
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
(eq? 'a 'a) ; #t
(eq? 'a 'b) ; #f

(define x 10)
(define y 10)
(eq? x y) ; #t

(define lst1 '(1 2 3))
(define lst2 '(1 2 3))
(eq? lst1 lst2) ; #f because they are different objects in memory

;equal?
; equal? used for deep comparison checking if they have the same structure and content 
(equal? 'a 'a) ; #t
(equal? 'a 'b) ; #f
; compare a list
(define lst1 '(1 2 3))
(define lst2 '(1 2 3))
(equal? lst1 lst2) ; #t
; compare a list with a different order
(define lst3 '(3 2 1))
(equal? lst1 lst3) ; #f
; compare a list with same content but different structure
(define lst4 '(1 (2 3)))
(define lst5 '(1 2 3))
(equal? lst4 lst5) ; #f


; List 
(list 10 20 30)    ; (10 20 30)
' (10 20 30)       ; (10 20 30)
(quote (10 20 30)) ; (10 20 30)
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
(define lst2 (list 'a 2 "Hello" 'world / #f + double 10 x 'y 'a #\H '(1 2 3) (+ 3 1)))      ; Evaluates each piece the expression
(define lst3 (quote ( 'a 2 "Hello" 'world / #f + double 10 x 'y 'a #\H (1 2 3) (+ 3 1) )))  ; Does not valuate the expression
; ( a  2 Hello  world  #<subr (/ arg1 :rest args)> #f #<subr (+ :rest args)> #<closure (double x)> 10 10 y a H (1 2 3) 4)
; ('a 2  Hello  'world /                           #f         + double 10 x 'y 'a H (1 2 3)                    (+ 3 1))
(display lst2)
(display lst3)

; List Operations
; car
(car '(1 2 3)) ; 1 - gets the head of the list
; cdr
(cdr '(1 2 3)) ; (2 3) - gets the tail of the list
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


; (load "test.scm")
(define (❤️)
    (print "I love functional programming."))


; regular recursion
(define (my-factorial n)
    (if (<= n 1)
        1
        (* n (my-factorial (- n 1)))))

; tail recursive recursion
(define (my-factorial_tr n)
    (let aux ((x n) (acc 1))
        (if (<= x 1)
            acc
        (aux (- x 1) (* x acc)))))

; recursion through list using loop 
(define (my-intersperse lst filler)
    (let loop ((lst lst) (acc '()))
    (if (null? lst)
        (reverse acc) ; reverse at the end to maintain the original order
        (if (null? (cdr lst)) 
            (loop '() (cons (car lst) acc)) ; add the last element without filler
            (loop (cdr lst) (cons filler (cons (car lst) acc)))))))

(define (IO)
    (display "Enter your name: ")
    (let ((name (read))) ;
        (display "Hello, ")
        (display name)
        (display "!")
        (newline)))

(define (fizz-buzz n)
    (define (helper i)
    (cond
        ((> i n) '())                       ; Stop when i > n
        ((and (= (modulo i 3) 0) (= (modulo i 5) 0)) 
        (cons "FizzBuzz" (helper (+ i 1)))) ; FizzBuzz for multiples of 3 and 5
        ((= (modulo i 3) 0) 
        (cons "Fizz" (helper (+ i 1))))     ; Fizz for multiples of 3
        ((= (modulo i 5) 0) 
        (cons "Buzz" (helper (+ i 1))))     ; Buzz for multiples of 5
        (else 
        (cons i (helper (+ i 1))))))        ; Otherwise, return the number
    (helper 1)                              ; Start at 1
)     

; two sum leetcode
(define (two-sum lst target)
(let loop1 ((lst1 lst) (i 0))
  (if (null? lst1)
      #f ; Return #f if no solution is found
      (let loop2 ((lst2 (cdr lst1)) (j (+ i 1)))
        (if (null? lst2)
            (loop1 (cdr lst1) (+ i 1)) ; Move to the next element in lst1
            (if (= (+ (car lst1) (car lst2)) target)
                (list i j) ; Return indices as a list
                (loop2 (cdr lst2) (+ j 1)))))))) ; Move to the next element in lst2

;; Example usage
(two-sum '(2 7 11 15) 9) ; => (0 1)

(define (valid-parentheses str)
(let loop ((i 0) (balance 0))
  (cond
    ((< balance 0) #f)                         ; More closing than opening
    ((>= i (string-length str)) (= balance 0)) ; End of string, balance must be 0
    ((char=? (string-ref str i) #\()           ; Open parenthesis
     (loop (+ i 1) (+ balance 1)))
    ((char=? (string-ref str i) #\))           ; Close parenthesis
     (loop (+ i 1) (- balance 1)))
    (else (loop (+ i 1) balance)))))           ; Ignore other characters (optional)


