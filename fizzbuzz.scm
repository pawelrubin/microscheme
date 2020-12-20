(define (print str)
    (display str)
    (newline))

(let fizz-buzz ((i 1))
    (if (>= 100 i)
        ; (begin
        (cond
            ((= 0 (modulo i 15)) (print 'fizzbuzz))
            ((= 0 (modulo i 3)) (print 'fizz))
            ((= 0 (modulo i 5)) (print 'buzz))
            (else (print i)))
        (fizz-buzz (+ i 1)))  )


; (cond ((= 0 1) (print 'dupa)) ((> 1 0) (print 'lol )) (else print 'xd))