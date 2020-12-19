(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #f)
      ((atom? (car l)) (lat? (cdr l)))
      (else #t))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l))#t)
      (else (member? a (cdr l))))))

(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (eq? a (car lat) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (eq? (car lat) new) (cons (cdr lat) old)(cons new lat)
      (else (cons (car lat) (insertR new old (cdr lat))))



(print (atom? 'a))
(print (lat? '(a b s)))
(print (member? 'a '(b c s)))
(print (rember 'fal '(hoge fal fuga piyo)))
(print (insertR 'fal 'test '(hoge fal fuga piyo)))