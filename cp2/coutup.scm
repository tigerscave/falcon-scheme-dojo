(define add1
  (lambda (n)
      (+ n 1)))

(define countup
  (lambda (a l)
    (cond 
      ((null? l) 0)
      ((eq? a (car l))  (add1 (countup a (cdr l))))
      (else (countup  a (cdr l))) )))

(define sub1
  (lambda (n)
      (- n 1)))

(define nums
  (lambda (n)
    (cond 
      ((zero? n) '())
      (else (cons n (nums (sub1 n)))))))

(print (countup 'apple '(a b apple a apple)))
(print (countup 'apple '(a b c)))
(print (add1 5))

(print (nums 1))
(print (nums 5))