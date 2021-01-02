; fib.scm
(define (fib n)
    (if (<= n 2)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

(define n (read))
(display (fib n))
(newline)
