(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat) (rember a (cdr lat)))))))))

(print (rember ' fal '()))
(print (rember 'and '(bacon lettuce and tomato)))
;(cons 'bacon '(rember 'and '(lettuce and tomato)))
;(cons 'bacon (cons 'lettuce (rember 'and '(and tomato))))
;(cons 'bacon (cons 'lettuce '(tomato)))
;(cons 'bacon '(lettuce tomato))
;'(bacon lettuce tomato)
(print "-------------------------------------------------------")




(define firsts
  (lambda (l)
    (cond
      ((null? l) quote ())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts '((a b) (c d) (e f)))
;(cons 'a (firsts '((c d) (e f))))
;(cons 'a (cons 'c (firsts '((e f)))))
;(cons 'a (cons 'c (cons 'e (firsts '()))))
;(cons 'a (cons 'c (cons 'e '())))
;(cons 'a (cons 'c '(e)))
;(cons 'a '(c e))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) old) (cons old (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))))