(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'a))
(print (atom? '(a)))

(define lat?
  (lambda (l)
  (cond 
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f))))

(print (lat? '(a v c)))
(print (lat? '(a (v) c)))

(define member?
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((eq? a (car l)) #t)
    (else (member? a (cdr l))))))

(print (member? 'a '(v a c)))
(print (member? 'a '(v b c)))


(define rember
  (lambda (a l)
  (cond
    ((null? l) (quote()))
    ((eq? a (car l)) (rember a (cdr l)))
    (else (cons (car l) (rember a (cdr l)))))))

(print (rember 'a '(n a c a s)))

(define firsts
  (lambda (l)
  (cond
    ((null? l) (quote()))
    (else (cons (car (car l)) (firsts (cdr l)))))))

(print (firsts '((a b) (c d) (r d))))

(define subst
  (lambda (old new l)
  (cond
    ((null? l) (quote ()))
    ((eq? (car l) old ) (cons new (subst old new (cdr l))))
    (else (cons (car l) (subst old new (cdr l)))))))

(print (subst 'a 'b '(c a c a)))

(define insertR
  (lambda (old new l)
  (cond
    ((null? l) (quote ()))
    ((eq? (car l) old ) (cons old (cons new (insertR old new (cdr l)))))
    (else (cons (car l) (insertR old new (cdr l)))))))
    
(print (insertR 'a 'b '(c a c a)))

(define insertL
  (lambda (old new l)
  (cond
    ((null? l) (quote ()))
    ((eq? (car l) old ) (cons new (cons old (insertL old new (cdr l)))))
    (else (cons (car l) (insertL old new (cdr l)))))))

(print (insertL 'a 'b '(c a c a)))