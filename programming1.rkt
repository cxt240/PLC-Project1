(define &&
  (lambda (a b)
    (cond
      ((or (null? a) (null? b)) '())
      ((and a b) #t)
      (else #f)
      )
    )
  )

(define ||
  (lambda (a b)
    (cond
      ((or (null? a) (null? b)) '())
      ((or a b) #t)
      (else #f)
      )
    )
  )

(define !
  (lambda (a)
    (cond
      ((null? a) '())
      (else (not a))
      )
    )
  )
                     