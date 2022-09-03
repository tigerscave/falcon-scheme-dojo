(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(print (add1 '1))
(print (sub1 '1))


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

(print (plus '1 '2))
(print (plus '4 '2))


(print (minus '4 '2))
(print (minus '5 '2))

(define addtup
  (lambda (tup)
  (cond
    ((null? tup) 0)
    (else (plus (car tup) (addtup (cdr tup)))))))

(print (addtup '(1 5 6)))

(define *
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (plus n (* n (sub1 m)))))))

(print (* '3 '2))
(print (* '6 '2))


(define tup+
  (lambda (tup1 tup2)
  (cond
    ((and (null? tup1) (null? tup2)) (quote ()))
    (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(print (tup+ '(1 2 3) '(1 2 3)))


(define >
  (lambda (n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (> (sub1 n) (sub1 m))))))

(print (> '5 '2))
(print (> '2 '5))
(print (> '5 '5))

(define =
  (lambda (n m)
  (cond
    ((> n m) #f)
    ((< n m) #f)
    (else #t))))

(print (= '1 '1))
(print (= '1 '2))
(print (= '3 '2))


(define =2
  (lambda (n m)
  (cond
    ((zero? n) (zero? m))
    ((zero? n) #f)
    (else (=2 (sub1 n) (sub1 m))))))

(define =3
  (lambda (n m)
  (cond
    ((zero? m) (zero? n))
    (else (=3 (sub1 n) (sub1 m))))))

(print '----------)
(print (=3 '5 '5))
(print (=3 '4 '2))
(print (=3 '3 '24))


(define factorial
  (lambda (n m)
  (cond
    ((zero? m) 1)
    (else (* n (factorial n (sub1 m)))))))

(print (factorial '2 '4))