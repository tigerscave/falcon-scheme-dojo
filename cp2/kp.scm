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
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))




(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (eq? (car lat) new) (cons (cdr lat) old)(cons new lat)
      (else (cons (car lat) (insertR new old (cdr lat))))

(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (eq? a (car lat) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))


(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))


(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))