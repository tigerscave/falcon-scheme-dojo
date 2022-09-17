(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'a))
(print (atom? '()))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #f)
      ((atom? (car l)) (lat? (cdr l)))
      (else #t))))


(print (lat? '(a b c)) 
;(print (lat? '(a (b) c)))

(define member?
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((eq? a (car l)) (member? a (cdr l)))
      (else #t))))

(print (member? 'a '(b c a)))

;note
