(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'a))
(print (atom? '()))

(define lat?
  (lambda (l)
  (cond 
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f))))

(print (lat? '()))
(print (lat? '((a))))

(define member?
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((eq? (car l) a) #t)
    (else (member? a (cdr l))))))

(print (member? 'a '(b a c)))
(print (member? 'a '(b b c)))