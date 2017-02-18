(load "simpleParser.scm")
(require racket/trace)
         
; checks if the variable exists
(define exists?
  (lambda (l x)
    (cond
      ((null? l) #f)
      ((eq? (car l) x) #t)
      (else (exists? (cdr l) x)))))

; gets the index of the corresponding value
(define getIndex
  (lambda (l x)
    (cond
      ((null? l) (error no value))
      ((eq? (car l) x) 0)
      (else (+ 1 (getIndex (cdr l) x))))))

; gets the value of the variable at index x
(define getValue
  (lambda (l x)
    (cond
      ((zero? x) (car l))
      (else (getValue (cdr l) (- x 1))))))

; changes the value of the variable in variable stack l given index x
(define setValue
  (lambda (l x)
    (cond
      ((zero? x) (cons x (cdr l)))
      (else (cons (car l) (setValue (cdr l) (- x 1)))))))


(define statement
  (lambda (stmt var val)
    (cond
      ((eq? 'while (car stmt)) 'while)
      ((eq? 'if (car stmt)) 'if)
      (else (varFunction stmt var val)))))

(define ifStmt
  (lambda (tfStmt stmt1 stmt2 var val)
    (cond
      ((compound tfStmt var val) (statement stmt1 var val))
      (else (statement stmt2 var val)))))

;(define while
 ; (lambda (tfStmt body var val)
  ;  (cond
   ;   ((compound tfStmt var val) (while tfStmt body (lambda ((statement body var val))))))));; not complete
 

(define varFunction
  (lambda (stmt var val)
    (cond
      ((eq? 'var (car stmt)) (declare (cadr stmt) var val))
      ((eq? '= (car stmt)) (assign (cadr stmt) var val))))) 

       
(define instr
  (lambda (l var val)
    (cond
      ((null? l) #f)
      ((exists? var 'return) (getValue val (getIndex var 'return)))
      (else (begin
              (statement (car l) var val)
              (instr (cdr l) var val))))))

;-------------------------------------------------------------------------------------------------
;
; Arithmetic Functions (written by Kavan)
;
;-------------------------------------------------------------------------------------------------

(define identify
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
      ((list? x) (identify var val)) ; list, just do another identify call
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

;--------------------------------------------------------------------------------------------
;
; Boolean operations (Andrew)
;
;--------------------------------------------------------------------------------------------

;identifys out boolean comparator operations
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

;identifys out int compator operations
(define simple
  (lambda (stmt var val)
    (cond
      ((eq? (car stmt) '==) (eq? (identify (cadr stmt) var val) (identify (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '!=) (not (eq? (identify (cadr stmt) var val) (identify (cadr (cdr stmt)) var val))))
      ((eq? (car stmt) '>)  (>   (identify (cadr stmt) var val) (identify (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '<)  (<   (identify (cadr stmt) var val) (identify (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '>=) (>=  (identify (cadr stmt) var val) (identify (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '<=) (<=  (identify (cadr stmt) var val) (identify (cadr (cdr stmt)) var val)))
      ((eq? (car stmt) '!)  (not (compound (cdr stmt) var val)))
      (else (error '(invalid boolean operation))))))