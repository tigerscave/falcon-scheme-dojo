; cp.1
; atom?
; lat?
; (member? a lat)

; cp.2
; (rember a lat)
; (firsts l)
; (insertR new old lat)
; (insertL new old lat)
; (subst new old lat)
; (multirember a lat)
; (multiinsertR new old lat)
; (multiinsertL new old lat)
; (multisubst new old lat)

; (firsts l)            (firsts '((a b) (c d) (e f))) => (a c f)
; (insertR new old lat)        (insertR 'a 'b '(a b c)) => (a b a c)
; (insertL new old lat)        (insertL 'a 'b '(a b c)) => (a a b c)
; (subst new old lat)        (subst 'a 'b '(a b c)) => (b b c)

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
(print (lat? '(a (8b) c)))

(define member?
  (lambda (a lat)
  (cond
    ((null? lat) #f)
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat))))))

(print (member? 'a '(b a c)))
(print (member? 'a '(b b c)))

(define rember
  (lambda (a lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? a (car lat)) (rember a (cdr lat)))
    (else (cons (car lat) (rember a (cdr lat)))))))

(define rember2
  (lambda (a lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? a (car lat)) (cdr lat))
    (else (cons (car lat) (rember2 a (cdr lat)))))))

(print (rember 'a '(b a c a)))
(print (rember 'a '(b b c a)))

(print (rember2 'a '(b a c a)))
(print (rember2 'a '(b b c a)))



(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (subst new old (cdr lat))))
    (else (cons (car lat) (subst new old (cdr lat)))))))

(print (subst 'a 'b '(c b b s)))


(define insertR
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons old (cons new (insertR new old (cdr lat)))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))

(print (insertR 'a 'b '(c b b s)))


(define insertL
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (cons old (insertL new old (cdr lat)))))
    (else (cons (car lat) (insertL new old (cdr lat)))))))

(print (insertL 'a 'b '(c b b s)))


(define insertR2
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR2 new old (cdr lat)))))))

(print (insertR2 'a 'b '(c b b s)))
(print (insertR2 'a 'b '()))