(define atom?
  (lambda (x)
    (and (not (pair? x))(not( null? x)))))

(define lat?
  (lambda (l)
  (cond 
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f))))

(define member
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((eq? a (car l)) #t)
    (else (member a (cdr l)))))) 
  
;--------------------------------------------

(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
            (rember a (cdr lat)))))))

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

(define subst
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) o1)
            (cons new (cdr lat)))
            ((eq? (car lat) o2)
            (cons new (cdr lat)))
            (else (cons (car lat)
                    (subst new o1 o2
                      (cdr lat)))))))))

(print "---atom?---")
(print (atom? '(l))) ;アトムかどうか判定
(print "---lat?---")
(print (lat? '(a b ))) ;リスト内にリストがあるか判定
(print "---member?---") ;引数aがリストbにあるか判定
(print (member 'apple '()))
(print (member 'a '(b a c)))