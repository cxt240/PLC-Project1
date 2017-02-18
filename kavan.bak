(define %
  (lambda (a b)
    (cond
      ( (or (null? a) (null? b))   '())
      (else (modulo a b) )
    )    
  )
)

(define ==
  (lambda (a b)
    (cond
      ( (or (null? a) (null? b)) '())
      ( (eq? a b) #t )
      (else #f)
    )
  )
)

(define filter
  (lambda (stmt var val)  ; car stmt is op
    (cond
      ( (or (null? stmt) (null? var) (null? val)) '() )
      (  (eq? '+ (car stmt)) (sum (filter  ) ( )  ))    ; + op
      (  (eq? '- (car stmt)) (diff (filter  ) ( )  ))    ; + op
      (  (eq? '* (car stmt)) (prod (filter  ) ( )  ))    ; + op
      (  (eq? '/ (car stmt)) (div (filter  ) ( )  ))    ; + op
      (  (eq? '% (car stmt)) (% (filter  ) ( )  ))    ; + op

    )
  )
)

(define sum
  (lambda (a b)
    (cond
      ( (or (null? a) (null? b)) 0)
      (else (+ a b))
    )
  )
)


(define diff
  (lambda (a b)
    (cond
       ( (or (null? a) (null? b)) '())
       (else (- a b)))))


(define prod
  (lambda (a b)
    (cond
       ( (or (null? a) (null? b)) '())
       (else (* a b)))))

(define div
  (lambda (a b)
    (cond
       ( (or (null? a) (null? b)) '())
       (else (/ a b)))))



      