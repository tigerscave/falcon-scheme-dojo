; cp.1
; atom?
; lat?
; (member? a lat)

; cp.2
; (rember a lat)                  (rember 'a '(b a c))) => (b c)
; (firsts l)                      (firsts '((a b) (c d) (e f))) => (a c f)
; (insertR new old lat)           (insertR 'a 'b '(a b c)) => (a b a c)
; (insertL new old lat)           (insertL 'a 'b '(a b c)) => (a a b c)
; (subst new old lat)             (subst 'a 'b '(a b c)) => (a a c)
; (multirember a lat)             (multirember 'a '(b a c a))) => (b c)
; (multiinsertR new old lat)      (multiinsertR 'a 'b '(a b c b)) => (a b a c b a)
; (multiinsertL new old lat)      (multiinsertL 'a 'b '(a b c b)) => (a a b c a b)
; (multisubst new old lat)        (multisubst 'a 'b '(a b c b)) => (a a c a)


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

(print (lat? '(a b c)))
(print (lat? '((a))))


(define rember
  (lambda (a lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? a (car lat)) (cdr lat))
    (else (rember a (cdr lat))))))

(print (rember 'a '(a b c a)))

(define firsts
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    (else (cons (car (car l)) (firsts (cdr l)))))))

(print (firsts '((a b) (c d) (e f))))


(define insertR
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))

(print (insertR 'a 'b '(c b c b)))
(define insertL
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (cons old (cdr lat))))
    (else (cons (car lat) (insertL new old (cdr lat)))))))

(print (insertL 'a 'b '(c b c b)))

(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat)))))))

(print (subst 'a 'b '(c b c b)))


(define multirember
  (lambda (a lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? a (car lat)) (multirember a (cdr lat)))
    (else (cons (car a) (multirember a (cdr lat)))))))

(print (multirember 'a '(b a c a)))



(define multiinsertR
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(print (multiinsertR 'a 'b '(c b c b)))

(define multiinsertL
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(print (multiinsertL 'a 'b '(c b c b)))

(define multisubst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat)))))))

(print (multisubst 'a 'b '(c b c b)))