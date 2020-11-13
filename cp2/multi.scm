

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) a)
          (multirember a (cdr lat)))
          (else (cons (car lat)
                  (multirember a
                    (cdr lat)))))))))
(print " ")
(print "---multirember---")
(print (multirember 'cup '(coffee cup tea cup and hick cup))) ;cupを全部抜く

;(cons coffee (multirember 'cup '(cup tea cup and hick cup)))
;(cons coffee (multirember 'cup '(tea cup and hick cup))
;(cons coffee (cons tea (multirember 'cup '(cup and hick cup))))
;(cons coffee (cons tea (multirember 'cup '(and hick cup)))
;(cons coffee (cons tea (cons and (multirember 'cup '(hick cup))
;(cons coffee (cons tea (cons and (cons hick (multirember 'cup '(cup))))))
;(cons coffee (cons tea (cons and (cons hick (multirember 'cup '())
;(cons coffee (cons tea (cons and (cons hick
;(coffee tea and hick)


(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
            (cons old
              (cons new
              (multiinsertR new old
                (cdr lat)))))
              (else (cons (car lat)
                      (multiinsertR new old
                        (cdr lat)))))))))
(print " ")
(print "---multiinsertR---")
(print (multiinsertR 'fried 'fish '(chips and fish or fish and fried))) ;fish全部の右側にfriedを配置

;(cons chips (multiinsertR 'fried 'fish '(and fish or fish and fried)))
;(cons chips (cons and (multiinsertR 'fried 'fish '(fish or fish and fried))))
;(cons chips (cons and (cons fish (cons fried (multiinsertR 'fried 'fish '(or fish and fried))))
;(cons chips (cons and (cons fish (cons fried (cons or (multiinsertR 'fried 'fish '(fish and fried)))
;(cons chips (cons and (cons fish (cons fried (cons or (cons fish (cons fried (multiinsertR 'fried 'fish '(and fried))) )
;(cons chips (cons and (cons fish (cons fried (cons or (cons fish (cons fried (cons and (multiinsertR 'fried 'fish '(fried)))
;(cons chips (cons and (cons fish (cons fried (cons or (cons fish (cons fried (cons and (cons fried (multiinsertR 'fried 'fish '()))
;(chips and fish fried or fish fried and fried)

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
            (cons new
              (cons old
              (multiinsertL new old
                (cdr lat)))))
              (else (cons (car lat)
                      (multiinsertL new old
                        (cdr lat)))))))))
(print " ")
(print "---multiinsertL---")
(print (multiinsertL 'fried 'fish '(chips and fish or fish and fried))) ;fish全部の左側にfriedを配置

;(cons chips (multiinsertL 'fried 'fish '(and fish or fish and fried)))
;(cons chips (cons and (multiinsertL 'fried 'fish '(fish or fish and fried))))
;(cons chips (cons and (cons fried (cons fish (multiinsertL 'fried 'fish '(or fish and fried))))
;(cons chips (cons and (cons fried (cons fish (cons or (multiinsertL 'fried 'fish '(fish and fried)))
;(cons chips (cons and (cons fried (cons fish (cons or (cons fried (cons fish (multiinsertL 'fried 'fish '(and fried))) )
;(cons chips (cons and (cons fried (cons fish (cons or (cons fried (cons fish (cons and (multiinsertL 'fried 'fish '(fried)))
;(cons chips (cons and (cons fried (cons fish (cons or (cons fried (cons fish (cons and (cons fried (multiinsertL 'fried 'fish '()))
;(chips and fried fish or fried fish and fried)


(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
            ((eq? (car lat) old)
              (cons new
                (multisubst new old
                  (cdr lat))))
          (else (cons (car lat)
                  (multisubst new old
                      (cdr lat)))))))))
(print " ")
(print "---multisubst---")
(print (multisubst 'fried 'fish '(chips and fish or fish and fried)) ;fishすべてをfriedに置き換え

;(cons chips (multisubst 'fried 'fish '(and fish or fish and fried))))
;(cons chips (cons and (multisubst 'fried 'fish '(fish or fish and fried))))
;(cons chips (cons and (cons fried (multisubst 'fried 'fish '(or fish and fried)))
;(cons chips (cons and (cons fried (cons or (multisubst 'fried 'fish '(fish and fried)))
;(cons chips (cons and (cons fried (cons or (cons fried (multisubst 'fried 'fish '(and fried)))
;(cons chips (cons and (cons fried (cons or (cons fried (cons and (multisubst 'fried 'fish '(fried)))
;(cons chips (cons and (cons fried (cons or (cons fried (cons and (cons fried (multisubst 'fried 'fish '()))
;(chips and fried or fried and fried)