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
    (and ( not (pair? x )) (not (null? x)))))

(print (atom? 'a))
(print (atom? '(a)))


(define lat?
  (lambda (l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f))))

(print (lat? '(a)))
(print (lat? '(a (b))))


(define member?
  (lambda (a lat)
  (cond
    ((null? lat) #f)
    ((eq? (car lat) a) #t)
    (else (member? a (cdr lat))))))

(print (member? 'a '(b c a)))
(print (member? 'a '(b c b)))


(define rember
  (lambda (a l)
  (cond
    ((null? l) (quote ()))
    ((eq? a (car l)) (rember a (cdr l)))
    (else (cons (car l) (rember a (cdr l)))))))

(print (rember 'a '(b c a d a)))
(print (rember 'a '(b c d)))


(define firsts
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    (else (cons (car (car l)) (firsts (cdr l)))))))

(print (firsts '((a b) (c d) (e f))))