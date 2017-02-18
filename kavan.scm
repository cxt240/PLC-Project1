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

(define sum
  (lambda (a b)
    (cond
      ( (or (null? a) (null? b)) '())
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



      