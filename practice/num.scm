(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (sub1 (minus n (sub1 m)))))))

(define addtup
  (lambda (tup)
  (cond
    ((null? tup) 0)
    (else (plus (car tup) (addtup (cdr tup)))))))

(define *
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (plus n (* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
  (cond
    ((and (null? tup1) (null? tup2)) (quote ()))
    (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
  (cond
    ((zero? n) #t)
    ((zero? m) #f)
    (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
  (cond
    ((> n m) #f)
    ((< n m) #f)
    (else #t))))

(define factorial
  (lambda (n m)
  (cond
    ((zero? m) 1)
    (else (* n (factorial n (sub1 m)))))))

(define /
  (lambda (n m)
  (cond
    ((< n m) 0)
    (else (add1 (/ (minus n m) m))))))

(define length
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l)))))))

(define pick
  (lambda (n l)
  (cond
    (= n (sub1 n)) (cdr l)
    (else (pick (sub1 n) l)))))

(define no-nums
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    ((number? (car l)) (no-nums (cdr l)))
    (else (cons (car l) (no-nums (cdr l)))))))

(define allnums
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    ((number? (car l)) (cons (car l) (allnums (cdr l))))
    (else (allnums (cdr l))))))

(define occur
  (lambda (a l)
  (cond
    ((null? l) 0)
    ((eq? a (car l)) (add1 (occur a (cdr l))))
    (else (occur a (cdr l))))))


(define tup+2
  (lambda (tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (plus (car tup1) (car tup2)) (tup+2 (cdr tup1) (cdr tup2)))))))

(print (tup+2 '(1 2 3) '(1 2)))


(define eqan?
  (lambda (a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2)))))

(print (eqan? 'a 'a))
(print (eqan? 'a 'b))
(print (eqan? '1 '1))
(print (eqan? '1 '2))


(define rempick
  (lambda (n l)
  (cond
    ((zero? (sub1 n)) (cdr l)) 
    (else (cons (car l) (rempick (sub1 n) (cdr l)))))))