(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
  (+ 1 n)))


(define sub1
  (lambda (n)
    (- n 1)))


(define rember*
  (lambda (a l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) a) (rember* a (cdr l)))
                 (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l))
                      (rember* a (cdr l)))))))

(define rember
  (lambda (a l)
  (cond
    ((null? l) (quote()))
    ((eq? a (car l)) (rember a (cdr l)))
    (else (cons (car l) (rember a (cdr l)))))))


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
    (else (length* (car l)) (length* (cdr l))))))

(define plus
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (add1 (plus n (sub1 m)))))))



(define sum
  (lambda (n)
  (cond
  ((null? n) 0)
  (else (plus (car n) (sum (cdr n)))))))

(define sum*
  (lambda (n)
  (cond
  ((null? n) 0)
  ((atom? (car n)) (plus (car n) (sum* (cdr n))))
  (else (sum* (car n)) (sum* (cdr n))))))


(print (length* '()))
(print (length* '(a b (c (d)) e)))
(print (plus 1 2))

(print (sum* '(1 2 3 4)))
(print (sum* '(1 2 (3) 4)))