; (print (tup+ '(0 1 2 3) '(10 11 12 13)))
; (10 12 14 16)

; (print (tup+ '(1 2) '(5 6)))
; (6 8)

; (print (tup+ '(3 5 7) '(9 11 13)))
; (12 16 20)


(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
  (cond
    ((zero? m) n)
    (else (add1 (plus n (sub1 m)))))))


(print (add1 2))
(print (plus 1 3))


(define tup+
  (lambda (tup1 tup2)
    (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(print (tup+ '(0 1 2 3) '(10 11 12 13)))

; Ex.1
; (print (sq-sum '(1 2 3))
; 14

; Ex.2
; (print (sq-sum '(1 3 5 7 9)))
; 165

; Ex. 3
; (print (sq-sum '(10 11 12 13 14 15 16 17 18 19 20)))
; 2585


(define *
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (plus n (* n (sub1 m)))))))

(print (* 4 3))

(define power
  (lambda (n m)
  (cond
    ((zero? m) 1)
    (else (* n (power n (sub1 m)))))))

(print (power 2 4))


(define sq-sum
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (plus (power (car l) (car l)) (sq-sum (cdr l)))))))

(print (sq-sum '(1 2 3)))

(define sq-sum
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (plus (power (car l) (car l)) (sq-sum (cdr l)))))))

(define sq-sum2
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (plus (* (car l) (car l)) (sq-sum2 (cdr l)))))))

(print (sq-sum2 '(1 2 3)))
(print (sq-sum2 '(1 3 5 7 9)))