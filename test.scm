(define x 0)
(define y (if x 1 2))
(set! y x)

(define (foo a b c d) 
    (< a b c d))

(foo x y 21 37)

; (define (foo x y)
;   (define (bar a b)
;     (+ x y a b)
;     )
;   (bar 21 37)
;   )
