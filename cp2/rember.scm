(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
              (rember a (cdr lat)))))))))

(define rember2
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
            (rember2 a (cdr lat)))))))

(print "-------")

(print (rember2 'fal '()))
(print (rember2 'fal '(fal)))

(print (rember2 'fal '(apple)))


(print (cons (car '(apple)) (rember2 'fal (cdr '(apple)))))
(print (cons 'apple (rember2 'fal '())))
(print (cons 'apple '()))

(print "---------")
(print (rember2 'fal '(apple banana fal)))

(print (cons 'apple (rember2 'fal '(banana fal))))

(print (cons 'apple (cons 'banana (rember2 'fal '(fal)))))

(print (cons 'apple (cons 'banana '())))

(print (cons 'apple '(banana)))

(print '(apple banana))


(print (rember2 'fal '(hoge fuga piyo)))


