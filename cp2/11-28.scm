(define add1
  (lambda (n)
      (+ n 1)))

(define sub1
  (lambda (n)
      (- n 1)))

(define o+
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (add1 (+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0) 
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define ×
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (× n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
    ((zero? m) (zero? n))
    ((zero? n) #f)
    (else (= (sub1 n) (sub1 m))))))
  

(print (= '1 '1))

(print (> '7 '6))
(print (< '5 '6))

(print (× '10 '4))
(print (tup+ '(1 4 5) '(4 6 8)))