(print "hello world")

(print (eq? 2 2))

; atom? lat?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(print (atom? 'a))
(print (lat? '(a b c (a))))
