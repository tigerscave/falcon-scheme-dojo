;atom?
;lat?
;member?

;firsts
;multirember
;multisubst
;multiinsertR


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
(print (lat? '((a))))

(define member?
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((eq? a (car l)) #t)
    (else (member? a (cdr l))))))

(print (member? 'a '(b a c)))
(print (member? 'a '(b b c)))


(define firsts
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    (else (cons (car (car l)) (firsts (cdr l)))))))

(print (firsts '((a b c) (c b c) (ba b c))))


(define multirember
  (lambda (a l)
  (cond
    ((null? l) (quote ()))
    ((eq? a (car l)) (multirember a (cdr l)))
    (else (cons (car l) (multirember a (cdr l)))))))

(print (multirember 'a '(a b c a)))

(define multisubst
  (lambda (new old l)
  (cond
    ((null? l) (quote ()))
    ((eq? old (car l)) (cons new (multisubst new old (cdr l))))
    (else (cons (car l) (multisubst new old (cdr l)))))))

(print (multisubst 'b 'a '(c b a a b a)))


(define multiinsertR
  (lambda (new old l)
  (cond
    ((null? l) (quote ()))
    ((eq? old (car l)) (cons old (cons new (multiinsertR new old (cdr l)))))
    (else (cons (car l) (multiinsertR new old (cdr l)))))))


(print (multiinsertR 'a 'b '(v b a b)))