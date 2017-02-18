(load "Interpreter2.scm")
 
(define filter
  (lambda (stmt var val)  ; car stmt is op
    (cond
      ( (or (null? stmt) (null? var) (null? val)) '() )
      (  (eq? '+ (car stmt)) (sum (check (cadr stmt) var val) (check (cadr (cdr stmt)) var val)))    ; + op
      (  (eq? '- (car stmt)) (diff (check (cadr stmt) var val) (check (cadr (cdr stmt)) var val)))    ; + op
      (  (eq? '* (car stmt)) (prod (check (cadr stmt) var val) (check (cadr (cdr stmt)) var val)))    ; + op
      (  (eq? '/ (car stmt)) (div (check (cadr stmt) var val) (check (cadr (cdr stmt)) var val)))    ; + op
      (  (eq? '% (car stmt)) (% (check (cadr stmt) var val) (check (cadr (cdr stmt)) var val)))    ; + op

    )
  )
)

;checks if the substatement is an atom or not
(define check
  (lambda (x var val)
    (cond
      ((list? x) (filter var val)) ; list, just do another filter call
      ((number? x) x)              ; number, return the number
      (else (getValue val (getIndex var x))))))   ; else get the value of the variable


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



      