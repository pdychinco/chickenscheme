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
    (let loop ((n n) (result 1))
        (if (<= n 1)
            result
        (loop (- n 1) (* n result)))))

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


