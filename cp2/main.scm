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

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

(print (atom? 'a))
(print (lat? '(a b c (a))))

(print "--member--")
(print (member? 'a '(a b c)))
(print (member? 'a '(b c d a)))