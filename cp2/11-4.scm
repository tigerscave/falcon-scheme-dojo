(define insertR 
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(print (insertR 'topping 'fudge '(ice cream with fudge for dessert) ))

;(cons ice (insertR 'topping 'fudge '(cream with fudge for dessert)))
;(cons ice (cons cream (insertR 'topping 'fudge '(with fudge for dessert)))
;(cons ice (cons cream (cons with (insertR 'topping 'fudge '(fudge for dessert)))
;(cons ice (cons cream (cons with (cons fudge (cons topping (for dessert)))

(print (insertL 'topping 'fudge '(ice cream with fudge for dessert) ))

;(print (cons 'ice '(insertL 'topping 'fudge '(cream with fudge for dessert))))
;(cons 'ice '(cons 'cream '(insertL 'topping 'fudge '(with fudge for dessert)))
;(cons 'ice '(cons 'cream '(cons 'with '(insertL 'topping 'fudge '(fudge for dessert)))
;(cons 'ice '(cons 'cream '(cons 'with '(cons 'topping '(fudge for dessert))