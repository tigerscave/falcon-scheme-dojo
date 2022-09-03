; cp.1
; atom?
; lat?
; (member? a lat)

; cp.2
; (rember a lat)
; (firsts l)
; (insertR new old lat)
; (insertL new old lat)
; (subst new old lat)
; (multirember a lat)
; (multiinsertR new old lat)
; (multiinsertL new old lat)
; (multisubst new old lat)

; (firsts l)            (firsts '((a b) (c d) (e f))) => (a c f)
; (insertR new old lat)        (insertR 'a 'b '(a b c)) => (a b a c)
; (insertL new old lat)        (insertL 'a 'b '(a b c)) => (a a b c)
; (subst new old lat)        (subst 'a 'b '(a b c)) => (b b c)


(define atom?
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))

(print (atom? 'aa ))
(print (atom? '(aa) ))

; (define lat?
;   (lambda (l)
;     ((cond
;       (null? l)
;       ((atom? (car l)))
;       )

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? l) #f)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
      

(print (lat? '()))

(print (lat? '(aa)))
(print (lat? 'a ))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(print (member? 'a '(b a c)))
(print (member? 'a '(b b c)))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (rember a (cdr lat)))
      (else (cons (car lat) (rember a (cdr lat)))))))

(print (rember 'a '(b a c)))
(print (rember 'a '(b b c)))
(print (rember 'a '()))
(print (rember 'a '(a b a b)))

; (firsts l)            (firsts '((a b) (c d) (e f))) => (a c e)

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))) )))

(print (firsts '((a b) (c d) (e f))))

; (insertR new old lat)        (insertR 'a 'b '(a b c)) => (a b a c)

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cons old (cdr lat))))
      (else (cons (car lat ) (insertR new old (cdr lat)))))))

(print (insertR 'a 'b '(a b c)))

(print (insertR 'a 'b '()))

(print (insertR 'a 'b '(b)))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat ) (insertL new old (cdr lat)))))))

(print (insertL 'a 'b '(a b c)))
(print (insertR 'a 'b '(a b c)))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old (cons old (cdr lat))))
      (else (subst new old (cdr lat))))))


(print (subst 'a 'b '(a b c)))
; (subst new old lat)        (subst 'a 'b '(a b c)) => (b b c)

  ;schemeの処理を一行ずつ紙に書いてみる
  ;来週シフト決まったら予定連携
  ;毎朝課題の関数を書く
  ;30分以内に全てかける
  ;英語完了まで
  ;オファーレター