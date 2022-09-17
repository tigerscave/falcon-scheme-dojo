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

(print (lat? '(a b c)))
(print (lat? '(a (b) c)))

(define member?
  (lambda (x lat)
  (cond
    ((null? lat) #f)
    ((eq? x (car lat)) #t)
    (else (member? x (cdr lat))))))

(print (member? 'b '(b)))

(print (member? 'a '(a))) ;#t
(print (member? 'apple '(a apple))) ;#f
(print (member? 'apple '(apple))) ;t

(define rember
  (lambda (a l)
  (cond
    ((null? l) (quote ()))
    ((eq? a (car l)) (rember a (cdr l)))
    (else (cons (car l) (rember a (cdr l)))))))

(print (rember 'a '(b a c)))

(define firsts
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    (else (cons (car (car l)) (firsts (cdr l)))))))

(print (firsts '((a))))
(print (firsts '((a c) (b c))))

;(firsts '((a b) (c d) (e f))) => (a c e)


(define insertR
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons old (cons new (insertR new old (cdr lat)))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))

;(insertR 'a 'b '(a b c)) => (a b a c)

(print (insertR 'a 'b '(a b c)))

(define insertL
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (cons old (insertL new old (cdr lat)))))
    (else (cons (car lat) (insertL new old (cdr lat)))))))

(print (insertL 'a 'b '(a b c)))

;(insertL 'a 'b '(a b c)) => (a a b c)

(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? old (car lat)) (cons new (subst new old (cdr lat))))
    (else (cons (car lat) (subst new old (cdr lat)))))))


;(subst 'a 'b '(a b c)) => (b b c)

(print (subst 'a 'b '(a b c)))
