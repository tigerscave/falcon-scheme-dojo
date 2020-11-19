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

(define o-
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0) 
      (else (o+ (car tup) (addtup (cdr tup)))))))

(print "---add1---") ;1を+
(print (add1 67))
(print "---sub1---") ;1を-
(print (sub1 67))
(print "---zero---") ;0か判定
(print (zero? 67))
(print "---o+---") ;引数を+
(print (o+ 67 1))
(print "---o----") ;引数を-
(print (o- 67 1))
(print "---addtup---") ;リスト内の合計
(print (addtup '(1 2 4)))