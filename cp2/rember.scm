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


(print (cons 'apple (rember2 'fal '(banana fal))))

(print (cons 'apple (cons 'banana (rember2 'fal '(fal)))))

(print (cons 'apple (cons 'banana '())))

(print (cons 'apple '(banana)))

(print '(apple banana))


(print (rember2 'fal '(hoge fuga piyo)))

(print (cons 'hoge (rember2 'fal '(fuga piyo))))
(print (cons 'hoge (cons 'fuga (rember2 'fal '(piyo)))))
(print (cons 'hoge (cons 'fuga (cons 'piyo (rember2 'fal '()))
(print (cons 'hoge (cons 'fuga (cons 'piyo '()))))

