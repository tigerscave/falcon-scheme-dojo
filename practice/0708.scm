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

(print (lat? '(a)))
(print (lat? '(a b (a))))


(define member?
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((eq? a (car l)) #t)
    (else (member? a (cdr l))))))

(print (member? 'a '(b a c)))
(print (member? 'a '(b b c)))

(define rember
  (lambda (a l)
  (cond
    ((null? l) (quote ()))
    ((eq? a (car l)) (rember a (cdr l)))
    (else (cons (car l) (rember a (cdr l)))))))

(print (rember 'a '(b a c)))
(print (rember 'a '(b a c a)))

(define firsts
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    (else (cons (car (car l)) (firsts (cdr l)))))))

(print (firsts '((a b c) (c b c) (b b c))))


(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons new (subst new old (cdr lat))))
    (else (cons (car lat) (subst new old (cdr lat)))))))


(print (subst 'a 'b '(b a c a)))


(define insertR
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons old (cons new (insertR new old (cdr lat)))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))

(print (insertR 'a 'b '(b a c a b)))