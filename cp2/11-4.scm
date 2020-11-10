(define insertR 
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertR2
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) old)
            (cons old
              (cons new (cdr lat))))
              (else (cons (car lat)
                      (insertR2 new old
                        (cdr lat)))))))))

(print (insertR2 'topping 'fudge '(ice cream with fudge for dessert) ))
;(cons ice (insertR2 topping fudge (cream with fudge for dessert))
;(cons ice (cons cream (insertR2 'topping 'fudge (with fudge for dessert))))
;(cons ice (cons cream (cons with (insertR2 'topping 'fudge '(fudge for dessert)))))
;(cons ice (cons cream (cons with (cons fudge (cons topping (for dessert)))
;(ice cream with fudge topping for dessert)



(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))


(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote()))
    (else (cond
          ((eq? (car lat) old)
          (cons new (cdr lat)))
          (else (cons (car lat)
                  (subst new old
                    (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) o1)
            (cons new (cdr lat)))
            ((eq? (car lat) o2)
            (cons new (cdr lat)))
            (else (cons (car lat)
                    (subst2 new o1 o2
                      (cdr lat)))))))))
            
(print (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping)))
;(cons vanilla (ice cream with chocolate topping))
;(vanilla ice cream with chocolate topping)

(print (subst2 'vanilla 'chocolate 'banana '(ice cream banana with chocolate topping)))
;(cons (ice (subst2 vanilla chocolate banana (cream banana with chocolate topping))))
;(cons (ice (cons cream (subst2 vanilla chocolate banana (banana with chocolate topping)))))
;(cons (ice (cons cream (cons vanilla (with chocolate topping) ))))
;(ice cream vanilla with chocolate topping)




(print (subst 'topping 'fudge '(ice cream with fudge for dessert)))




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