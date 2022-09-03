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

(define sum
  (lambda (n)
  (cond
  ((null? n) 0)
  (else (plus (car n) (sum (cdr n)))))))

(print (sum '(0)))
(print (sum '(1 2 3)))
(print (sum '(1 5 3)))