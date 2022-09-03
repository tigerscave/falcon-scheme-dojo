(define sum
  (lambda (l)
  (cond
  ((null? l) 0)
  (else (+ (car l) (sum (cdr l)))))))

(define add1
  (lambda (n)
  (+ 1 n)))

(define sub1
  (lambda (n)
    (- n 1)))


(define plus
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (add1 (plus n (sub1 m)))))))

(print (sum '(1 2 3)))

(define atom?
  (lambda (x)
  (and (not (pair? x))) (not (null? x))))

(print (atom? 'a))

(define sum*
  (lambda (l)
  (cond
  ((null? l) 0)
  ((atom? (car l)) (+ (car l) (sum* (cdr l))))
  (else (+ (sum* (car l)) (sum* (cdr l)))))))


(define length
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l)))))))

(define length*
  (lambda (l)
  (cond
    ((null? l) 0)
    ((atom? (car l)) (add1 (length* (cdr l))))
    (else (+ (length* (car l)) (length* (cdr l)))))))

(print (sum* '((0))))
(print (sum* '(1 (0))))
(print (length '(a a a)))
(print (length* '(a (a) (((a))))))