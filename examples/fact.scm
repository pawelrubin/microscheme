; fact.scm
(define (fact n)
    (if (= n 0)
        1
        (* (fact (- n 1)) n)))

(display (fact 5))
(newline)
