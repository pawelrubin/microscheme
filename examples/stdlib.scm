(define (not x) 
    (if x #f #t))

(define (null? obj)
    (if (eqv? obj '()) #t #f))

(define (list . objs) 
    objs)

(define (id obj) 
    obj)

(define (flip f)
    (lambda (x y) (f y x)))

(define (curry f x)
    (lambda (arg) (apply f (cons x (list arg)))))

(define (compose f g)
    (lambda (arg) (f (apply g arg))))

(define zero? 
    (curry = 0))

(define positive? 
    (curry > 0))

(define negative? 
    (curry < 0))

(define (odd? num)
    (= (mod num 2) 1))

(define (even? num)
    (= (mod num 2) 0))

(define (foldr func end lst)
    (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func acc lst)
    (if (null? lst)
        acc
        (foldl func (func acc (car lst)) (cdr lst))))

(define fold foldl)

(define reduce foldr)

(define (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

(define (sum . lst) (fold + 0 lst))

(define (map func lst)
    (foldr (lambda (x xs) (cons (func x) xs)) '() lst))

(define (filter pred lst)
   (foldr (lambda (x xs) (if (pred x) (cons x xs) xs)) '() lst))

"Scheme standard library"
