; concat
; (concat '(a b) '(c d))
; => (a b c d)

; (concat '(a b) '())
; => (a b)


(define concat
  (lambda (lat1 lat2)
    (cond 
      ((null? lat1) lat2)
      (else (cons (car lat1) (concat (cdr lat1) lat2))))))

(print (concat '(a b) '()))
(print (concat '(a b) '(c d)))