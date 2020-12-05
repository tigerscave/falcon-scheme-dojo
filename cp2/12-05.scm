(define add1
  (lambda (n)
      (+ n 1)))

(define sub1
  (lambda (n)
      (- n 1)))

(define length
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond

      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
    ((null? lat) (quote ()))
    (else 
      (cond
        ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))))


(print (length '(atom lat rember))) ;アトムの塊の数
(print (pick '3 '(lat atom test))) ;n番目の文字を出力
(print (rempick '3 '(falcon atom lambda))) ;n番目の文字を取り除く
(print (number? '3)) ;数字なら#tそれ以外は#f
(print (no-nums '(falcon 7 atom 8 lat))) ;数字をすべて取り除く
(print (all-nums '(lat 7 8 get at9 7))) ;数字を取り出す