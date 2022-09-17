; atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x) ))))

(print (atom? 'a))
(print (atom? '()))

(define lat?
  (lambda (l)
  (cond 
    ((null? l) #t)  ;判定
    ((atom? (car l)) (lat? (cdr l))) ;アトムなら右辺へ
    (else #f)))) 

(print (lat? '(a b c (z))))
(print (lat? '((z,1,a))))


(print '-----------------------------)



(define atom?2
  (lambda (x)
    (and (not (pair? x)) (not (null? x) ))))


(print (atom?2 'a))
(print '-----------------------------)

(define lat2
  (lambda (l)
  (cond 
    ((null? l) #t)
    ((atom? (car l)) (lat2 (cdr l)))
    (else #f))))

(print (lat2 '(a b c)))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l))#t) ;eq?なら終了
      (else (member? a (cdr l))))))

(print (member? 'fal '()))
(print (member? 'a '(b a c)))







(print '-----------------------------)








(define member?2
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l))#t)
      (else (member?2 a (cdr l))))))


(print (member?2 'fal '()))
(print (member?2 'a '(b a c)))
