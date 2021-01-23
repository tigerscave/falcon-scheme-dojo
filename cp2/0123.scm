(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'a))

(define lat?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat)) (lat? (cdr lat)))
      (else #t))))

(print (lat? '(a b c)))
(print (lat? '(a (b) c)))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(print (multirember 'a '(b c a d a)))

(define add1
  (lambda (n)
    (+ n 1)))

(define countup
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (countup a (cdr lat))))
      (else (countup a (cdr lat))))))

(print (countup 'apple '()))
(print (countup 'apple '(apple)))
(print (countup 'apple '(banana apple orange apple)))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(print (length '(a b c d e)))
(print (length '(a b c)))