(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'a))
(print (atom? '(a)))



(define rember*
  (lambda (a l)
  (cond
    ((null? l) (quote ()))
    ((atom? (car l))
    (cond
      ((eq? (car l) a) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(print (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)))