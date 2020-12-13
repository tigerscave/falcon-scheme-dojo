(define add1
  (lambda (n)
      (+ n 1)))

(define sub1
  (lambda (n)
      (- n 1)))

(define eqan?
  (lambda (a1 a2)
    (cond 
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2))#f)
      (else (eq? a1 a2)))))


(define occur
  (lambda (a lat)
    (cond
      ((null? lat)0)
      (else
        (cond
          ((eq? (car lat) a)
          (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
      ((zero? n)#f)
      (else (zero? (sub1 n))))))

(define rempick
  (lambda (n lat)
    (cond 
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(print (eqan? '3 '3)) ;同じ引数なら真（=は数、eq?はアトム）
(print (occur 'a '(a s c))) ;aがラット内に何個あるか
(print (one? '1)) ;1なら真それ以外は偽
(print (rempick '2 '(f g k))) ;ラットからn番目のアトムを取り除く
