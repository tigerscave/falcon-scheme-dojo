(define rember2p
  (lambda (a1 a2 lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
              (rember a (cdr lat)))))))))
(print (rember2p 'apple 'banana '()))
(print "---")

(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
              (rember a (cdr lat)))))))))
  

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) quote ())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) old)
              (cons old 
                (cons new (cdr lat))))
            (else (cons (car lat)
                    (insertR new old
                      (cdr lat)))))))))

(print "----")
(print (rember ' a '()))
(print (rember 'and '(bacon lettuce and tomato)))
(print "---------")
(cons 'bacon '(rember 'and '(lettuce and tomato)))
(cons 'bacon (cons 'lettuce (rember 'and '(and tomato))))
(cons 'bacon (cons 'lettuce '(tomato)))
(cons 'bacon '(lettuce tomato))
;'(bacon lettuce tomato)
(print "----aaaa-----")

(firsts '((a b) (c d) (e f)))
(cons 'a (firsts '((c d) (e f))))
(cons 'a (cons 'c (firsts '((e f)))))
(cons 'a (cons 'c (cons 'e (firsts '()))))
(cons 'a (cons 'c (cons 'e '())))
(cons 'a (cons 'c '(e)))
(cons 'a '(c e))
;'(a c e)
(print "----bbbb----")

(print (firsts ' ((g h) (i j) (k l))))
(print (firsts ' ((ab cd) (ef gh) (ij kl))))

(print (rember ' tes '(bacon lettuce and tomato)))
(print (rember ' dojo '(fal scheme dojo xxx)))