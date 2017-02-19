(load "simpleParser.scm")
(require racket/trace)

; checks if the given value is an atom. written in class 1-25-17
(define atom?
  (lambda (x)
      (and (not (pair? x)) (not (null? x)))))

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
  (lambda (l val x)
    (cond
      ((zero? x) (cons val (cdr l)))
      (else (cons (car l) (setValue (cdr l) (- x 1)))))))


(define statement
  (lambda (stmt stack)
    (cond
      ((eq? 'while (car stmt)) 'while)
      ((eq? 'if (car stmt)) (ifStmt (cadr stmt) (cadr (cdr stmt)) (cadr (cdr (cdr stmt))) stack))
      (else (varFunction stmt stack)))))

(define ifStmt
  (lambda (tfStmt stmt1 stmt2 stack)
    (cond
      ((compound tfStmt stack) (statement stmt1 stack))
      (else (statement stmt2 stack)))))

;(define while
 ; (lambda (tfStmt body stack)
  ;  (cond
   ;   ((compound tfStmt stack) (while tfStmt body (lambda ((statement body stack))))))));; not complete
 

(define varFunction
  (lambda (stmt stack)
    (cond
      ((eq? 'var (car stmt)) (declare (cadr stmt) stack))
      ((eq? '= (car stmt)) (assign (cadr stmt) (cadr (cdr stmt)) stack))
      ((eq? 'return (car stmt)) 'return)
      (else (error invalid statement)))))
 
(define declare
  (lambda (var stack)
    (cond
      ((null? var) (error invalid variable))
      (else (list (cons var (car stack)) (cons '0 (cadr stack)))))))

(define assign
  (lambda (var val stack)
    (cond
      ((list? val) (list (car stack) (setValue (cadr stack) (identify val stack) (getIndex (car stack) var))))
      ((exists? (car stack) var) (list (car stack) (setValue (cadr stack) val (getIndex (car stack) var))))
      (else (error invalid variable)))))

(define return
  (lambda (stmt stack)
    (cond
      ((null? stmt) (error bad statement))
      ((assign 'return (identify (cadr stmt) stack) (declare 'return stack))))))
                                              
(define instr
  (lambda (l stack)
    (cond
      ((null? l) #f)
      ((exists? (car stack) 'return) (getValue (cadr stack) (getIndex (car stack) 'return)))
      (else (instr (cdr l) (statement (car l) stack))))))

;-------------------------------------------------------------------------------------------------
;
; Arithmetic Functions (written by Kavan)
;
;-------------------------------------------------------------------------------------------------

(define identify
  (lambda (stmt stack)  ; car stmt is op
    (cond
      ((or (null? stmt)) '() )
      ((atom? stmt) (check stmt stack)) 
      ((eq? '+ (car stmt)) (+ (check (cadr stmt) stack) (check (cadr (cdr stmt)) stack)))    ; + op
      ((eq? '- (car stmt)) (- (check (cadr stmt) stack) (check (cadr (cdr stmt)) stack)))    ; - op
      ((eq? '* (car stmt)) (* (check (cadr stmt) stack) (check (cadr (cdr stmt)) stack)))    ; * op
      ((eq? '/ (car stmt)) (/ (check (cadr stmt) stack) (check (cadr (cdr stmt)) stack)))    ; / op
      ((eq? '% (car stmt)) (% (check (cadr stmt) stack) (check (cadr (cdr stmt)) stack)))    ; % op
    )))

;checks if the substatement is an atom or not
(define check
  (lambda (x stack)
    (cond
      ((list? x) (identify x stack)) ; list, just do another identify call
      ((number? x) x)              ; number, return the number
      (else (getValue (cadr stack) (getIndex (car stack) x))))))   ; else get the value of the variable


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
  (lambda (stmt stack)
    (cond
      ((eq? (car stmt) '&&) (and (compound ((cadr stmt) stack)) ((compound (cadr (cdr stmt)) stack))))
      ((eq? (car stmt) '||) (or  (compound ((cadr stmt) stack)) ((compound (cadr (cdr stmt)) stack))))
      ((eq? (car stmt) #f) #f)
      ((eq? (car stmt) #t) #t)
      (else (simple stmt stack))
      )
    )
  )

;identifys out int compator operations
(define simple
  (lambda (stmt stack)
    (cond
      ((eq? (car stmt) '==) (eq? (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '!=) (not (eq? (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack))))
      ((eq? (car stmt) '>)  (>   (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '<)  (<   (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '>=) (>=  (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '<=) (<=  (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '!)  (not (compound (cdr stmt) stack)))
      (else (error '(invalid boolean operation))))))