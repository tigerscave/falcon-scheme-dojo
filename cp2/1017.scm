; atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x) ))))

(print (atom? 'a))

; lat?

(define lat
  (lambda (l)
  (cond 
    ((null? l) #t)
    ((atom? (car l)) (lat (cdr l)))
    (else #f))))

(print (lat '(a b c (z))))



; member?
(define member
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((eq? a (car l)) #t)
    (else (member a (cdr l))))))
(print (member 'apple '()))
(print (member 'a '(b a c)))
