(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(print (sub1 '1))
(print (add1 '1))

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

(print (plus '1 '3))
(print (minus '1 '3))


(define *
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (plus n (* n (sub1 m)))))))

(print (* '3 '3))

(define tup
  (lambda (tup1)
  (cond
  ((null? tup1) 0)
  (else (plus (car tup1) (tup (cdr tup1)))))))

  (print (tup '(1 3 5)))


  (define >
    (lambda (n m)
    (cond
      ((zero? m) #t)
      ((zero? n) #f)
      (else (> (sub1 n) (sub1 m))))))
  
(print (> '3 '4))
(print (> '4 '3))

  (define <
    (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))


(print (< '3 '4))
(print (< '4 '3))


(define =
  (lambda (n m)
  (cond
    ((< n m) #f)
    ((< n m) #f)
    (else #t))))

(print (= '3 '4))
(print (= '3 '3))

(define factorial
  (lambda (n m)
  (cond
    ((zero? m) 1)
    (else (* n (factorial n (sub1 m)))))))

(define *2
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (plus n (*2 n (sub1 m)))))))

(define plus2
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (add1 (plus2 n (sub1 m)))))))


(define minus2
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (sub1 (minus2 n (sub1 m)))))))


(print (factorial '2 '4))
(print (*2 '2 '4))
(print (plus2 '2 '4))
(print (minus2 '2 '4))

(define qs
  (lambda (n m)
  (cond
    ((< n m) 0)
    (else (add1 (qs (minus n m) m))))))

(print (qs '2 '4))
(print (qs '4 '4))
(print (qs '5 '2))

(define length
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l)))))))


(print (length '(a b c d)))


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

(print (no-nums '(p 3 c 3 4 d)))


(define allnums
  (lambda (l)
  (cond
    ((null? l) (quote ()))
    ((number? (car l)) (cons (car l) (allnums (cdr l))))
    (else (allnums (cdr l))))))

(print (allnums '(p 3 c 3 4 d)))

(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(print (atom? '1))
(print (atom? 'a))
(print (atom? '(a)))


(define occur
  (lambda (a l)
  (cond
    ((null? l) 0)
    ((eq? a (car l)) (add1 (occur a (cdr l))))
    (else (occur a (cdr l))))))

(print (occur 'a '(b a c a c a)))