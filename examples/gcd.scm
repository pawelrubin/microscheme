(define (gcd a b) 
    (if (= b 0)
        a
        (gcd b (% a b))))

(display (gcd -4 14))
(newline)
