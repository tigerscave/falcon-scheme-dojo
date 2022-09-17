(define add1
  (lambda (n)
  (+ n 1)))

(define sub1
  (lambda (n)
  (- n 1)))


(print (add1 '2))
(print (sub1 '2))

(print (null? '()))

(print ((null? '()) quote()))