; hanoi.scm
(define (hanoi n)
    (if (= n 1)
        1
        (+ (* 2 (hanoi (- n 1))) 1)))

(define n (read))
(display (hanoi n))
(newline)
