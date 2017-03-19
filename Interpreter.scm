;-----------------------------------------------------------------------------------------------------
;
; EECS 345 Project 1
; Group Memebers: Chris Tsuei, Kavan Mally, Andrew Su
;
;-----------------------------------------------------------------------------------------------------

(load "simpleParser.scm")
(require racket/trace)

; ----------------------------------------------------------------------------------------------------
;
; Stack operations
;
; ----------------------------------------------------------------------------------------------------

; checks if the given value is an atom. written in class 1-25-17
(define atom?
  (lambda (x)
      (and (not (pair? x)) (not (null? x)))))

; checks if the variable exists
(define exists?
  (lambda (l x)
    (cond
      ((null? l) #f)               ; nothing left in list, so the variable does not exist
      ((eq? (car l) x) #t)         ; variable is in the list, return true
      (else (exists? (cdr l) x))))); recursive call to next element

; gets the index of the corresponding value
(define getIndex
  (lambda (l x)
    (cond
      ((null? l) (error "Using before declaring")) ; nothing left in list, throw error
      ((eq? (car l) x) 0)                          ; found variable, return zero and allow recursive add
      (else (+ 1 (getIndex (cdr l) x))))))         ; recursive call to cdr of list

; gets the value of the variable at index x
(define getValue
  (lambda (l x)
    (cond
      ((zero? x) (if (eq? 'null (car l)) (error "Using before assigning") (car l)))  ; if index is zero, return the character
      (else (getValue (cdr l) (- x 1))))))                                           ; recursive call with index - 1 and cdr of list

; changes the value of the variable in variable stack l given index x
(define setValue
  (lambda (l val x)
    (cond
      ((zero? x) (cons val (cdr l)))                                ; index zero, change variable here
      (else (cons (car l) (setValue (cdr l) val (- x 1)))))))       ; recursive call so that you reach index 0

; ---------------------------------------------------------------------------------------------------
;
; anything requiring a boolean operation to run
;
; ---------------------------------------------------------------------------------------------------

; if function that takes the condition, the then and else statements and a stack
(define ifStmt
  (lambda (tfStmt stmt1 stmt2 stack)
    (cond
      ((compound tfStmt stack) (statement stmt1 stack)) ; condition true, execute stmt1
      ((null? stmt2) stack)                             ; is there a statment 2, if not, return the stack
      (else (statement stmt2 stack)))))                 ; condition false, execute stmt 2

; while function that takes the condition, the body of the loop and the stack
(define while
  (lambda (tfStmt body stack)
    (cond
      ((compound tfStmt stack) (while tfStmt body (statement body stack))) ; if the loop condition is true, execute the body statement
      (else stack))))                                                      ; otherwise return the stack

; -------------------------------------------------------------------------------------------
;
; Variable declaration and assignment
;
;--------------------------------------------------------------------------------------------
; assign, declare and return filter, otherwise the statement is invalid
(define varFunction
  (lambda (stmt stack)
    (cond
      ((eq? 'var (car stmt)) (declare stmt stack))                          ; variable declaration
      ((eq? '= (car stmt)) (assign (cadr stmt) (cadr (cdr stmt)) stack))    ; assignment operation
      ((eq? 'return (car stmt)) (return stmt stack))                        ; return statment (creates a return variable, which is filtered in hte interpreter
      (else (error "invalid statement")))))

; Declaring variable (initializes variable to 0 if undeclared)
(define declare
  (lambda (stmt stack)
    (cond
      ((exists? (car stack) (cadr stmt)) (error "Redefining"))              ; variable has already been declared
      ((eq? 3 (length stmt)) (assign (cadr stmt) (cadr (cdr stmt)) (list (cons (cadr stmt) (car stack)) (cons 'null (cadr stack))))) ; variable with a value declaration
      (else (list (cons (cadr stmt) (car stack)) (cons 'null (cadr stack)))))))                      ; add var and init to stack in respective places

; Assigning a value to a variable given the variable name, the value to be assigned (can be a function), and the stack
(define assign
  (lambda (var val stack)
    (cond
      ((not (exists? (car stack) var)) (error "Using before declaring"))          ; variable does not exists
      ((list? val) (if (bool-op (car val)) (list (car stack) (setValue (cadr stack) (compound val stack) (getIndex (car stack) var)))
       (list (car stack) (setValue (cadr stack) (identify val stack) (getIndex (car stack) var)))))    ; if the assignment is to be a function, find the value of the function before changing the value
      ((exists? (car stack) var) (list (car stack) (setValue (cadr stack) (identify val stack) (getIndex (car stack) var))))       ; otherwise assign the value
      (else (error '(invalid variable))))))                                                                       ; else throw an error     

; ------------------------------------------------------------------------------------------------------------
;
; goto functions
;
; ------------------------------------------------------------------------------------------------------------

; return function, creates a variable called return
(define return
  (lambda (stmt stack)
    (cond
      ((exists? (car stack) 'return) stack)                                         ;if return already exists, do nothing
      ((list? (cadr stmt)) (if (bool-op (car (cadr stmt)))
                               (if (compound (cadr stmt) stack) (assign 'return 'true (declare '(var return) stack)) (assign 'return 'false (declare '(var return) stack)))
                               (assign 'return (identify (cadr stmt) stack) (declare '(var return) stack))))
      ((assign 'return (if (eq? (identify (cadr stmt) stack) #t) 'true
                           (if (eq? (identify (cadr stmt) stack) #f) 'false (identify (cadr stmt) stack)))
               (declare '(var return) stack))))))    ; initialize a return value and assign the return value

; handles bracket blocks by running each op in block until null
(define begin
  (lambda (body stack)
    (cond
      ((null? body) (statement body stack))
      (else stack)
    )
  )
)


; ------------------------------------------------------------------------------------------------------------
;
; Main interpreter
;
; ------------------------------------------------------------------------------------------------------------

; interpreter, takes a list of instructions and a blank stack ie '(() ())                                              
(define instr
  (lambda (l stack)
    (cond
      ((exists? (car stack) 'return) (getValue (cadr stack) (getIndex (car stack) 'return)))    ; if there's a return, just return the value, no more computation needed
      ((null? l) stack)                                                                  ; no return in instruction
      (else (instr (cdr l) (statement (car l) stack))))))             -                          ; else execute current instruction and do a recursive call for the next one

; basic filter for instructions, filters out while and if statements, otherwise fowarded to varFunction
(define statement
  (lambda (stmt stack)
    (cond
      ((null? stmt) stack)
      ((eq? 'while (car stmt)) (while (cadr stmt) (cadr (cdr stmt)) stack))                           ; while function call
      ((eq? 'if (car stmt)) (if (eq? 4 (length stmt))
                                (ifStmt (cadr stmt) (cadr (cdr stmt)) (cadr (cdr (cdr stmt))) stack)  ; if-then-else function call
                                (ifStmt (cadr stmt) (cadr (cdr stmt)) '() stack)))                    ; if-then function call
      ;add begin cond
      ((eq? 'begin (car stmt)) (begin (cdr stmt) stack) )
      ;add try cond
      ;( (eq? 'try (car stmt)) () )
      (else (varFunction stmt stack)))))                                                              ; send to varFunction

;-------------------------------------------------------------------------------------------------
;
; Arithmetic Functions (written by Kavan)
;
;-------------------------------------------------------------------------------------------------

; valid operator checker (for return statement) from the class notes
(define valid-op
  (lambda (op)
    (or (eq? op '+)
        (eq? op '-)
        (eq? op '*)
        (eq? op '/)
        (eq? op '%))))

;runs arithmetic operation based on sign
(define identify
  (lambda (stmt stack)  ; car stmt is op
    (cond
      ((or (null? stmt)) '() )
      ((atom? stmt) (check stmt stack)) 
      ((eq? '+ (car stmt)) (+ (check (cadr stmt) stack) (if (not (eq? 2 (length stmt))) (check (cadr (cdr stmt)) stack) 0)))    ; + op
      ((eq? '- (car stmt)) (- (if (eq? 2 (length stmt)) 0 (check (cadr stmt) stack))
                              (if (eq? 2 (length stmt)) (check (cadr stmt) stack) (check (cadr (cdr stmt)) stack))))            ; - op (or i guess negative sign)
      ((eq? '* (car stmt)) (* (check (cadr stmt) stack) (if (not (eq? 2 (length stmt))) (check (cadr (cdr stmt)) stack) 0)))    ; * op
      ((eq? '/ (car stmt)) (/ (check (cadr stmt) stack) (if (not (eq? 2 (length stmt))) (check (cadr (cdr stmt)) stack) 0)))    ; / op
      ((eq? '% (car stmt)) (modulo (check (cadr stmt) stack) (if (not (eq? 2 (length stmt))) (check (cadr (cdr stmt)) stack) 0)))    ; % op
      )))

;checks if the substatement is an atom or not
(define check
  (lambda (x stack)
    (cond
      ((list? x) (identify x stack)) ; list, just do another identify call
      ((number? x) x); number, return the number
      ((eq? 'false x) 'false)
      ((eq? 'true x) 'true)
      (else (getValue (cadr stack) (getIndex (car stack) x))))))   ; else get the value of the variable

;--------------------------------------------------------------------------------------------
;
; Boolean operations (Andrew)
;
;--------------------------------------------------------------------------------------------

; Checks for a boolean (true / false) operation
(define bool-op
  (lambda (op)
    (or (eq? op '&&)
        (eq? op '||)
        (eq? op '==)
        (eq? op '!=)
        (eq? op '>)
        (eq? op '>=)
        (eq? op '<)
        (eq? op '<=)
        (eq? op '!))))

;identifys out boolean comparator operations
(define compound
  (lambda (stmt stack)
    (cond
      ((atom? stmt) (if (eq? stmt 'true) #t (if (eq? stmt 'false) #f)))
      ((eq? (car stmt) #f) #f)
      ((eq? (car stmt) #t) #t)
      ((atom? stmt) (getValue (cadr stack) (getIndex (car stack) stmt)))
      ((eq? (car stmt) '&&) (and (compound (cadr stmt) stack) (compound (cadr (cdr stmt)) stack)))
      ((eq? (car stmt) '||) (or  (compound (cadr stmt) stack) (compound (cadr (cdr stmt)) stack)))
      (else (simple stmt stack)))))

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
      (else (error "invalid boolean operation")))))