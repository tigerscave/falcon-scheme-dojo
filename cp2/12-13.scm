(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))




(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(print (rember* 'chips '(chips ((with) fish) (chips)) ))
(print (rember* 'chips '(((with) fish) (chips)) ))
(print (cons (rember* 'chips '((with) fish)) (rember* 'chips '((chips)) )))
(print
  (cons
    (cons (rember* 'chips '(with)) (rember* 'chips '(fish)))
    (cons (rember* 'chips '(chips)) (rember* 'chips '()) )))


(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(print (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup ) ))

(print (member* 'chips '(chips ((with) fish) (chips))))
(print (or #t (member* 'chips '(((with) fish) (chips)))))
(print (or #t (or (member* 'chips '((with) fish)) (member* 'chips '((chips))) )))
(print
  (or #t
    (or
      (or (member* 'chips '(with)) (member* 'chips '(fish)))
      (or (member* 'chips '(chips)) (member* 'chips '())))))

;aaaa