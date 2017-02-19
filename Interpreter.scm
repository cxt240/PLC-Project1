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

; basic filter for instructions, filters out while and if statements, otherwise fowarded to varFunction
(define statement
  (lambda (stmt stack)
    (cond
      ((eq? 'while (car stmt)) (while (cadr stmt) (cadr (cdr stmt)) stack))                        ; while function call
      ((eq? 'if (car stmt)) (ifStmt (cadr stmt) (cadr (cdr stmt)) (cadr (cdr (cdr stmt))) stack))  ; if function call
      (else (varFunction stmt stack)))))                                                           ; send to varFunction

; if function that takes the condition, the then and else statements and a stack
(define ifStmt
  (lambda (tfStmt stmt1 stmt2 stack)
    (cond
      ((compound tfStmt stack) (statement stmt1 stack)) ; condition true, execute stmt1
      (else (statement stmt2 stack)))))                 ; condition false, execute stmt 2

; while function that takes the condition, the body of the loop and the stack
(define while
  (lambda (tfStmt body stack)
    (cond
      ((compound tfStmt stack) (while tfStmt body (statement body stack))) ; if the loop condition is true, execute the body statement
      (else stack))))                                                      ; otherwise return the stack
 

; assign, declare and return filter, otherwise the statement is invalid
(define varFunction
  (lambda (stmt stack)
    (cond
      ((eq? 'var (car stmt)) (declare (cadr stmt) stack))                   ; variable declaration
      ((eq? '= (car stmt)) (assign (cadr stmt) (cadr (cdr stmt)) stack))    ; assignment operation
      ((eq? 'return (car stmt)) (return stmt stack))                        ; return statment (creates a return variable, which is filtered in hte interpreter
      (else (error '(invalid statement))))))

; Declaring variable (initializes variable to 0)
(define declare
  (lambda (var stack)
    (cond
      ((null? var) (error '(invalid variable)))                           ; no variable declared, throw error
      (else (list (cons var (car stack)) (cons '0 (cadr stack)))))))      ; add var and init to stack in respective places

; Assigning a value to a variable given the variable name, the value to be assigned (can be a function), and the stack
(define assign
  (lambda (var val stack)
    (cond
      ((list? val) (list (car stack) (setValue (cadr stack) (identify val stack) (getIndex (car stack) var))))    ; if the assignment is to be a function, find the value of the function before changing the value
      ((exists? (car stack) var) (list (car stack) (setValue (cadr stack) val (getIndex (car stack) var))))       ; otherwise assign the value
      (else (error '(invalid variable))))))                                                                       ; else throw an error     

; return function, creates a variable called return
(define return
  (lambda (stmt stack)
    (cond
      ((exists? (car stack) 'return) stack)                                         ;if return already exists, do nothing
      ((assign 'return (identify (cadr stmt) stack) (declare 'return stack))))))    ; initialize a return value and assign the return value

; interpreter, takes a list of instructions and a blank stack ie '(() ())                                              
(define instr
  (lambda (l stack)
    (cond
      ((null? l) '(no return))                                                                  ; no return in instruction
      ((exists? (car stack) 'return) (getValue (cadr stack) (getIndex (car stack) 'return)))    ; if there's a return, just return the value, no more computation needed
      (else (instr (cdr l) (statement (car l) stack))))))                                       ; else execute current instruction and do a recursive call for the next one

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
      ((eq? (car stmt) '&&) (and (compound (cadr stmt) stack) (compound (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '||) (or  (compound (cadr stmt) stack) (compound (cadr (cdr stmt)) stack)))
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
      ((eq? (car stmt) '!)  (not (compound (cadr stmt) stack)))
      (else (error '(invalid boolean operation))))))