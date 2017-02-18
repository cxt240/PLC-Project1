(load "Interpreter2.rkt")
(load "kavan.scm")

;filters out boolean comparator operations
(define compound
  (lambda (stmt var val)
    (cond
      ((eq? (car stmt) '&&) (and (compound ((cadr stmt) var val)) ((compound (cadr (cdr stmt)) var val))))
      ((eq? (car stmt) '||) (or  (compound ((cadr stmt) var val)) ((compound (cadr (cdr stmt)) var val))))
      ((eq? (car stmt) #f) #f)
      ((eq? (car stmt) #t) #t)
      (else (simple stmt var val))
      )
    )
  )

;filters out int compator operations
(define simple
  (lambda (stmt var val)
    (cond
      ((eq? (car stmt) '==) (eq? (filter (cadr stmt) var val) (filter (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '!=) (not (eq? (filter (cadr stmt) var val) (filter (cadr (cdr stmt)) var val))))
      ((eq? (car stmt) '>)  (>   (filter (cadr stmt) var val) (filter (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '<)  (<   (filter (cadr stmt) var val) (filter (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '>=) (>=  (filter (cadr stmt) var val) (filter (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '<=) (<=  (filter (cadr stmt) var val) (filter (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '!)  (not (compound (cdr stmt) var val)))
      (else (error '(invalid boolean operation)))
      )
    )
  )


      

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
                     