(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
      (+ n 1)))


(define sub1
  (lambda (n)
      (- n 1)))

(define o+
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (add1 (+ n (sub1 m)))))))



(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))

;

;(print (occur* 'chips '(chips ((with) fish) (chips)) ))

;(add1 (occur* 'chips '(((with) fish) (chips))))

;(add1
;  (+
;     (occur* 'chips '((with) fish)) -> 0
;     (occur* 'chips '((chips))) -> 1
;))

;(add1 (+ 0 1))
;(add1 1)
;2

;(occur* 'chips '((with) fish)) -> 0
;(+ (occur* 'chips '(with)) (occur* 'chips '(fish)))
;(+ (occur* 'chips '()) (occur* 'chips '()))
;(+ 0 0)

;(occur* 'chips '((chips)))
;(+ (occur* 'chips '(chips)) (occur* 'chips '()))
;(+ (add1 (occur* 'chips '())) (occur* 'chips '()))
;(+ (add1 0) 0) -> 1

;1 (((with) fish) (chips))
;1 ((with) fish) ((chips))
;	1 (with) (fish)
;	1 (chips) ()
							
(define insertR*
	(lambda (new old l)
		(cond
			((null? l) (quote ()))
			((atom? (car l))
				(cond
					((eq? (car l) old)
					 (cons old (cons new (insertR* new old (cdr l)))))
			(else (cons (car l) (insertR* new old (cdr l))))))
			(else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(print (occur* 'chips '(chips ((with) fish) (chips)) ))
(print (insertR* 'fish 'chips '(chips 'fish ((with) fish) (chips)) ))

(define subst*
	(lambda (new old l)
		(cond
			((null? l) (quote ()))
			((atom? (car l))
			(cond
				((eq? (car l) old)
				(cons new (subst* new old (cdr l))))
			(else (cons (car l) (subst* new old (cdr l))))))
		(else
			(cons (subst* new old (car l)) (subst* new old (cdr l)))))))


(print (subst* 'fal 'chips '(chips ((with) fish) (chips)) ))
(cons 'fal '(((with) fish) (chips)) )
(cons 'fal '(((with) fish) (chips)) ) (cons '((with) fish) '((chips)) )
	(cons '(with) '(fish)
	(cons '(chips) '())