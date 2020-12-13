(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)) )))

(print (atom? 'a))

(define lat?
  (lambda (l) 
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(print (lat? '((a) v ) ))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)(quote ()))
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat)))) )))

(print (multirember 'apple '(test apple test apple)))