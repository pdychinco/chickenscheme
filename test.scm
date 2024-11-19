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
    (let ((name (read)))
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

; continuation example 
(define (find-first-even lst)
    (call/cc                       ; Capture the current continuation
     (lambda (exit)
       (for-each 
        (lambda (x)
          (display (string-append "Checking: " (number->string x)))
          (newline)
          (if (even? x)
              (exit x)             ; Exit immediately and return the first even number
              #f))                 ; Continue if the number is not even
        lst)
       'no-even-numbers)))         ; Return this if no even number is found
  
  (display (find-first-even '(1 3 5 7 4 9))) ; Outputs "Checking: 1", "Checking: 3", etc., and returns 4
  (newline)
  (display (find-first-even '(1 3 5 7 9)))   ; Outputs "Checking: 1", "Checking: 3", etc., and returns "no-even-numbers"
  