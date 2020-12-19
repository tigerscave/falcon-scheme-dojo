(define atom?
    (lambda (x)
        (and (not (pair? x) )(not (null? x)))))

(define lat?
    (lambda (l)
        (cond
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

(define )

(print (atom? 'apple))
(print (lat? '(a b (c))))
(print (lat? '()))
(print (lat? '(apple)))