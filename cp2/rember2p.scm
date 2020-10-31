
(define rember2p
    (lambda (a1 a2 lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? a1 (car lat)) (rember2p a1 a2 (cdr lat)))
            ((eq? a2 (car lat)) (cdr lat))
            (else (cons (car lat) (rember2p a1 a2 (cdr lat))))
        )
    )
)


(print (rember2p 'a 'b '()))
(print (rember2p 'a 'b '(a)))  
(print (rember2p 'a 'b '(b))) 
(print (rember2p 'a 'b '(c)))

(print (rember2p 'a 'b '(a c d)))
(print (rember2p 'a 'b '(b c d)))

(print (rember2p 'a 'b '(z x a g b)))
;(cons z (rember2p 'a 'b '(x a g b))
;(cons z (cons x (rember2p 'a 'b '(a g b))
;(cons z (cons x (rember2p 'a 'b '(g b)))
;(cons z (cons x (cons g (rember2p 'a 'b '(b)))))
;(cons z (cons x (cons g () )))
;(cons z (c))


;(cons z (cons x (rember2p 'a 'b '(g b))) (cons g (rember2p 'a 'b '(b)))


;(cons z (cons x (g b)
;(z x g b)
; (z x g)