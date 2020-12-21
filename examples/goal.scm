(define (list . objs) 
    objs)

(define (curry f x)
    (lambda (arg) (apply f (cons x (list arg)))))

(define (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

(define (npows2 n) 
    (unfold (curry * 2) 1 (curry = (^ 2 n))))
