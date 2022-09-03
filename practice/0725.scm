;(length* '())
; => 0
;(length* '(a b c))
; => 3
;(length* '(a b (c (d))))
; => 4

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define add1
  (lambda (n)
  (+ 1 n)))

(print (add1 1))

(define length
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l)))))))

(print (length '(a b c)))
(print (length '()))

; (define length*
;   (lambda (l)
;   (cond
;     ((null? l) 0)
;     (cond
;     (atom? (car l))
;     (else (add1 (length* (cdr l)))))
;     (else (length* (car l)) (length* (cdr l))))))

; (define length*
;   (lambda (l)
;   (cond
;     ((eq? l 5) #t)
;     ((eq? l 1) #f)
;     (cond
;     (else 123))
;     (else 456))))

; (print (length* 1))
; (print (length* 5))
; (print (length* 3))


(define length*
  (lambda (l)
  (cond
    ((null? l) 0)
    ((atom? (car l)) (add1 (length* (cdr l))))
    (else (length* (car l)) (length* (cdr l))))))


(print (length* '()))
(print (length* '(a b (c (d)))))