(define (duplicate lst)
(if (null? lst)
    '()
    (cons (car lst) (cons (car lst) (duplicate (cdr lst)) ))))

; 1 Map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (my-map func lst)
    (if (null? lst)
        '()
        (cons (func (car lst)) (my-map func (cdr lst )))))

; Map Example
;(define (square x) (* x x))
; (map square '(1 2 3 4 5))       ; (1 4 9 16 25)


; 2 Filter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (filter pred lst)
(cond
    ((null? lst) 
        '() )
    ((pred (car lst)) 
        (cons (car lst) (filter pred (cdr lst))) )
    (else
        (filter pred (cdr lst)) )  
)
)

; Filter example:
(define (my-even? x) (= 0 (modulo x 2)))
; (display (filter even? '(1 2 3 4 5 6)))  ; (2 4 6)


; 3 Fold L ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (foldL func acc lst)
(if (null? lst)
    acc
    (foldL func (func acc (car lst)) (cdr lst))))

; FoldL Example:
(define (add x y) (+ x y))
; (display (foldL add 0 '(1 2 3 4 5)))  ; 15


; 4 Compose ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (my-compose funcA funcB)
    (lambda (x) (funcA (funcB x))))

; Compose Example
(define (add2 x) (+ x 2))
(define (square x) (* x x))
(define add2-square (compose square add2))
;(add2-square 3) ; 25




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


