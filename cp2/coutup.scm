(define add1
  (lambda (n)
      (+ n 1)))

(define countup
  (lambda (a l)
    (cond 
      ((null? l) 0)
      ((eq? a (car l))  (add1 (countup a (cdr l))))
      (else (countup  a (cdr l))) )))

(define make-nums
  (lambda (n)
    (cond 
      ((null? n) 0)
      ((eq? (add1 3) (car n)) (make-nums (cdr n)))
      (else (cons (add1 (make-nums n)))))))

(print (countup 'apple '(a b apple a apple)))
(print (countup 'apple '(a b c)))
(print (add1 5))

(print (make-nums '(4)))