(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'a'))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(print (lat? '((aaa) bbbb cccc)))
(print (lat? '(aaa bbbb cccc)))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)))