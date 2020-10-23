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
            (rember a (cdr lat)))))))

(print (rember 'and '(bacon lettuce and tomato)))
(print (rember 'fal '(falcon scheme dojo)))
(print (rember 'falcon '(falcon scheme dojo)))
(print (rember2 'falcon '(falcon scheme dojo falcon)))
(print (rember2 'scheme '(falcon scheme dojo scheme)))
(print (rember2 'fal '(falcon scheme dojo)))