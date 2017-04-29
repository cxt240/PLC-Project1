;-----------------------------------------------------------------------------------------------------
;
; EECS 345 Project 3
; Group Memebers: Chris Tsuei, Kavan Mally, Andrew Su
;
;-----------------------------------------------------------------------------------------------------

;command issues:
;(interpret (parser "filename.txt"))

(load "classParser.scm")
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

; suffix returns a list from the original list l containing elements after the last occurence in l of the atom a
(define suffix
  (lambda (a l)
    (letrec ([last? (lambda (a1 l1 return)      ; checks if the current list has no atoms a1 remaining
                              (cond
                                ((null? l1) #t)        ; null list, then true
                                ((eq? a1 (car l1)) #f) ; if there's an a1 remaining in l1, false
                                (else (last? a1 (cdr l1) (lambda (v) (return v))))))] ; cps call to the next element
             [next (lambda (a l return)                ; finds the list after the next element of x
                                       (cond
                                         ((null? l) '())           ; nothing left, return an empty list
                                         ((eq? a (car l)) (cdr l)) ; found atom a, return the cdr of the list
                                         (else (next a (cdr l) (lambda (v) (return v))))))]) ; otherwise cps call to the next element
      (if (last? a l (lambda (v) v)) l (suffix a (next a l (lambda (v) v)))))))              ; body statement using letrec. if statement checks if there's no atoms a
                                                                                             ; in l, if true, returns the list, otherwise recursive call using the list
                                                                                            ; outputted by the next function

; tells if a variable is in the scope of the function
(define localScope
  (lambda (x a)
    (cond
      ((null? x) #f)                        ; nothing left in the list, return false
      ((eq? 'function (car x)) #f)          ; function seperator, return false
      ((eq? (car x) a) #t)                  ; matching variable, return true
      (else (localScope (cdr x) a)))))      ; check with the cdr of the list

; tells if variable is in the inner scope
(define inner?
  (lambda (x a)
    (cond
      ((null? x) #f)                        ; nothing left, false
      ((eq? 'innerfunction (car x)) #f)     ; function seperator, return false
      ((eq? (car x) a) #t)                  ; matching variable, return true
      (else (inner? (cdr x) a)))))      ; recursive call

; in the scope of the variable.
(define inScope
  (lambda (x a)
    (or (exists? (suffix 'function x) a) (localScope x a))))

; checks if the variable exists
(define exists?
  (lambda (l x)
    (cond
      ((null? l) #f)               ; nothing left in list, so the variable does not exist
      ((eq? (car l) x) #t)         ; variable is in the list, return true
      (else (exists? (cdr l) x))))); recursive call to next element

; gets the index of the corresponding value
(define index
  (lambda (l x)
    (cond
      ((null? l) (error "Using before declaring")) ; nothing left in list, throw error
      ((eq? (car l) x) 0)                          ; found variable, return zero and allow recursive add
      (else (+ 1 (index (cdr l) x))))))         ; recursive call to cdr of list

(define getIndex
  (lambda (l x)
    (cond
      ((localScope l x) (index l x))
      (else (+ (index (suffix 'function l) x) (- (length l) (length (suffix 'function l))))))))

                      
; gets the value of the variable at index x
(define getValue
  (lambda (l x)
    (cond
      ((zero? x)
       (if (eq? 'null (car l))
           (error "Using before assigning") (car l)))  ; if index is zero, return the character
      (else (getValue (cdr l) (- x 1))))))             ; recursive call with index - 1 and cdr of list

; changes the value of the variable in variable stack l given index x
(define setValue
  (lambda (l val x)
    (cond
      ((zero? x) (cons val (cdr l)))                    ; index zero, change variable here
      (else (cons (car l)
                  (setValue (cdr l) val (- x 1)))))))   ; recursive call so that you reach index 0

; ---------------------------------------------------------------------------------------------------
;
; layer operations (Chris Tsuei)
;
; ---------------------------------------------------------------------------------------------------

; adds a layer of "var"
(define addLayer
  (lambda (var l)
    (cond
      (else (list (cons var (car l)) (cons var (cadr l)))))))

; destroys all layers up to the layer containing "var"
(define destroyLayer
  (lambda (var l)
    (cond
      ((atom? l) l)                                            ; atom ---  if list is an atom, return
      ((atom? (car l)) (cons (car l) (destroyLayer var (cdr l))))      ; break/continue atom, pop the stack
      ((eq? 'throw (caar l)) (cons (car l) (destroyLayer var (cdr l)))); throw (it's the first list) pop stack
      ((eq? 'return (caar l)) (list
                               (cons 'return (removeX (car l) (index (car l) var)))
                               (cons (caadr l) (removeX (cadr l) (index (car l) var)))))
      ((zero? (index (car l) var))
       (list (cdr (car l)) (cdr (cadr l))))                    ; if layer index 0 return list of sublist 1 and 2
      (else (list
             (removeX (car l) (index (car l) var))
             (removeX (cadr l) (index (car l) var)))))))

; function removes the xth layer 
(define removeX
  (lambda (l x)
    (cond
      ((null? l) '())                              ; null case empty list
      ((zero? x) (cdr l))                          ; no layers left return remaining of list
      (else (removeX (cdr l) (- x 1))))))          ; layers left, recursive call

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
    (letrec ([loop (lambda (condition body stack2)
                     (call/cc (lambda (cc)
                                    (cond
                                      ((atom? stack2) stack2)                                                ; if stack atom return atom
                                      ((eq? 'break (car stack2)) (cc (cdr stack2)))                          ; break, exit loop (remove break from stack)
                                      ((eq? 'throw (car (car stack2))) (cc stack2))                          ; throw, exit loop (return stack, including the throw) ;((throw excetpionValue) (variables ...) (values ...))
                                      ((compound tfStmt stack2) (loop tfStmt body (statement body stack2)))  ; if the loop condition is true, execute the body statement
                                      (else stack2)))))])                                                    ; otherwise return the stack
      (loop tfStmt body stack))))

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
      ((eq? '= (car stmt)) (set (cadr stmt) (caddr stmt) stack))    ; assignment operation
      ((eq? 'return (car stmt)) (return stmt stack))                        ; return statment (creates a return variable, which is filtered in hte interpreter
      (else (error "invalid statement")))))

(define set
  (lambda (var val stack)
    (cond
      ((list? val) (if (eq? (car val) 'funcall) (setVar var (runFunction (cadr val) (cddr val) stack))
                       (assign var (identify val stack) stack)))
      (else (assign var val stack)))))

; Declaring variable (initializes variable to 0 if undeclared)
(define declare
  (lambda (stmt stack)
    (cond
      ((eq? 3 (length stmt))
       (assign (cadr stmt) (caddr stmt)
               (list (cons (cadr stmt) (car stack)) (cons 'null (cadr stack)))))            ; variable with a value declaration
      (else (list (cons (cadr stmt) (car stack)) (cons 'null (cadr stack)))))))             ; add var and init to stack in respective places

;
(define setVar
  (lambda (var stack)
    (assign var (getReturn stack) (list (cdar stack) (cdadr stack)))))

; Assigning a value to a variable given the variable name, the value to be assigned (can be a function), and the stack
(define assign
  (lambda (var val stack)
    (cond
      ((not (exists? (car stack) var)) (error "Variable not in scope"))                                                            ; variable does not exist
      ((list? val)
       (cond
         ((bool-op (car val)) (list (car stack) (setValue (cadr stack) (compound val stack) (getIndex (car stack) var))))
         ((and (list? (car val)) (eq? (caar val) 'throw)) val)
         (else (list (car stack) (setValue (cadr stack) (identify val stack) (getIndex (car stack) var))))))                                      ; if the assignment is to be a function, find the value of the function before changing the value
      ((exists? (car stack) var) (if (inScope (car stack) var)
                                     (list (car stack) (setValue (cadr stack) (identify val stack) (getIndex (car stack) var)))             ; otherwise assign the value
                                     (error "Variable not in scope")))
      (else (error '(invalid variable))))))                                                                                              ; else throw an error     

; ------------------------------------------------------------------------------------------------------------
;
; goto functions
;
; ------------------------------------------------------------------------------------------------------------

; return function, creates a variable called return
(define return
  (lambda (stmt stack)
    (cond
      ((exists? (car stack) 'return) stack)                                                   ;if return already exists, do nothing
      ((list? (cadr stmt)) (if (bool-op (car (cadr stmt)))
        (if (compound (cadr stmt) stack) (assign 'return 'true (declare '(var return) stack))
          (assign 'return 'false (declare '(var return) stack)))
          (assign 'return (identify (cadr stmt) stack) (declare '(var return) stack))))      ; true/false return 
      ((assign 'return (if (eq? (identify (cadr stmt) stack) #t) 'true
        (if (eq? (identify (cadr stmt) stack) #f) 'false (identify (cadr stmt) stack)))
          (declare '(var return) stack))))))                                                 ; initialize a return value and assign the return value

; handles bracket blocks by running each op in block until null
;@param: body of the loop/block; stack for all variables and values pairs
(define begin
  (lambda (body stack)
    (cond
      ((atom? stack) stack)                                   ; if stack atom return atom
      (else (destroyLayer 'layer (instr body (addLayer 'layer stack)))))))         ; else pop layer

; ------------------------------------------------------------------------------------------------------------
;
; Try/catch/finally
;
; ------------------------------------------------------------------------------------------------------------

; try catch finally call/cc function
(define tcf
  (lambda (l stack)
    (letrec ([try (lambda (body stack1)
                    (call/cc (lambda (cc)
                               (cond
                                 ((null? body) (cc stack1))                                         ; no more commands, return stack
                                 ((atom? (car stack1)) (cc stack1))                                 ; some return value, return stack + value
                                 ((eq? 'throw (car (car stack))) (cc stack1))                       ; something thrown, return the previous stack + throw message
                                 (else (try (cdr body)                                              ; main try function
                                            (with-handlers([exn:fail? (lambda (exn) (cc stack1))])  ; handle exceptions in the next command, (if error in next
                                              (statement (car body) stack1))))))))]                 ; instruction, return current stack)
             [catch (lambda (body x stack1)
                      (call/cc (lambda (cc)
                                 (cond
                                   ((eq? 'throw (caar stack1))                                 ; if something was thrown, add a layer, assign the error (e) the thrown value
                                    (destroyLayer 'layer (begin body                                           ; run catch body on it
                                                     (assign x (cadar stack1) (declare (list 'var x) (addLayer 'layer (cdr stack1))))))) 
                                   ((null? body) (cc stack1))                                       ; no body, return stack
                                   (else (cc stack1))))))]                                          ; return stack otherwise
             [finally (lambda (body stack1)
                        (call/cc (lambda (cc)
                                   (cond
                                     ((null? body) (cc stack1))                                     ; nothing to run, return stack
                                     ((atom? (car stack1)) (cc stack1))                             ; stack has a return, return it
                                     (else (cc (instr body stack1)))))))])                          ; otherwise run finally block
      (cond
        ((null? (cadr l)) (finally (cadr (cadr (cdr l))) (try (car l) stack)))                      ; no catch, run try and finally           
        (else (finally (if (null? (cadr (cdr l))) '() (cadr (cadr (cdr l))))                        ; no finally, run try/catch
               (catch (cadr (cdr (cadr l))) (car (cadr (cadr l))) (try (car l) stack))))))))        ; try catch finally

; ------------------------------------------------------------------------------------------------------------
;
; function stuff
;
; ------------------------------------------------------------------------------------------------------------

(define popReturn
  (lambda (stack)
    (if (exists? (car stack) 'function) (list (cdar stack) (cdadr stack)) stack)))

(define getReturn
  (lambda (stack)
    (cond
      ((eq? 'return (caar stack)) (caadr stack))
      (else stack))))

       
; adds all functions that were declared as well as their parameters and instructions to the stack
(define funcFilter
  (lambda (l stack)
    (cond
      ((null? l) stack)                                                     ; nothing else to add, return the stack
      ((eq? 'var (caar l)) (funcFilter (cdr l) (declare (car l) stack)))    ; if the current value is a global variable
      (else (funcFilter (cdr l) (list (cons (cadar l) (car stack)) (cons (cddar l) (cadr stack)))))))) ; adding a function

; assigns the specified inputs to the parameters of a function
(define paramAssign
  (lambda (field param newStack stack)
    (cond
      ((null? field) newStack)                                                ; no more parameters to assign
      (else (paramAssign (cdr field) (cdr param)                              ; next parameter to assign, add current one to stack
                         (list (cons (car field) (car newStack)) (cons (identify (car param) stack) (cadr newStack))) stack)))))
    
; function call and run
(define runFunction
  (lambda (name params stack)
    (cond
      ((not (inScope (car stack) name)) (error "Function not declared"))                                 ; function doesn't exist, throw an error
      ((localScope (car stack) name)
          (destroyLayer 'innerfunction (instr (cadr (getValue (cadr stack) (getIndex (car stack) name)))                     ; inner functions
                           (paramAssign (car (getValue (cadr stack) (getIndex (car stack) name))) params (addLayer 'innerfunction stack) stack))))
      (else (if (eq? (length (car (getValue (cadr stack) (getIndex (car stack) name)))) (length params)) ; if the number of fields are the same as the number of parameters
                (destroyLayer 'function (instr (cadr (getValue (cadr stack) (getIndex (car stack) name)))               ; run instruction after declaring and assigning values
                                 (paramAssign (car (getValue (cadr stack) (getIndex (car stack) name))) params (addLayer 'function stack) stack)))
                (error "Entered parameters don't match declared parameters"))))))

; -------------------------------------------------------------------------------------------
;
; Class and object stuff (Kavan)
;
;--------------------------------------------------------------------------------------------


; get instructions from main class (Kavan)
(define getClassMethods
  (lambda (class l)
    (cond
      ((null? l) '())                                   ;no class of such 
      ((eq? class (car(cdr(car l))) )   (cdr(cdr(car l))) )          ; class stuff has the desired class name
      (else (addMainClass class (cdr l)))               ; try again with other class
    )
  )
)



; if there is extends then add functions to the stack (except main). Assuming result from addMainClass passed here
; (addExtends '(extends B) (parse "file" "class"))
;(define addExtends
;  (lambda (extendedClass )
;    (cond
;      ( (eq? 'extends (car (car l))) (addMainClass (cdr (car l))) ()  )
;    )
;  )
;)

; check if (ex B extends A) A.method exists in B.method. If so, remove


; ------------------------------------------------------------------------------------------------------------
;
; Main interpreter
;
; ------------------------------------------------------------------------------------------------------------

; main interpreter function, runs all instructions
(define interpret
  (lambda (l)
    (method l (addLayer 'layer '(() ())))))

; parses the file and adds fields to the stack and sends it to the main method handler
(define method
  (lambda (l stack)
    (main (funcFilter l (addLayer 'base stack)))))

; runs the main method
(define main
  (lambda (stack)
    (if (exists? (car stack) 'main) (run (cadr (getValue (cadr stack) (getIndex (car stack) 'main))) (addLayer 'function stack)) ; checks if the main method exists
        (error "no main method"))))

; process return from main function interpret
(define run
  (lambda (l stack)
    (cond 
      ((atom? stack) stack)                               ; if the stack is an atom (it's the return value)
      ((eq? 'return (caar stack)) (getReturn stack))
      ((null? l) (error "no return"))                     ; no return, throw error
      ((eq? 'throw (caar stack)) (error "illegal throw")) ; something thrown, but not caught
      (else (run (cdr l) (instr l stack))))))             ; run instructions

; main interpreter, takes a list of instructions and a blank stack ie '(() ())                                              
(define instr
  (lambda (l stack)
    (cond
      ((atom? stack) stack)                                                                     ; there's a return, return it
      ((eq? 'continue (car stack)) (cdr stack))                                                 ; continue, return the stack
      ((eq? 'break (car stack)) stack)                                                          ; break, return stack
      ((eq? 'throw (caar stack)) stack)                                                         ; throw return stack
      ((exists? (car stack) 'return) stack)                                                     ; if there's a return, return the stack
      ((null? l) stack)                                                                         ; no return in instruction
      (else (instr (cdr l) (statement (car l) stack))))))                                       ; else execute current instruction and do a recursive call for the next one

; basic filter for instructions, filters out while and if statements, otherwise fowarded to varFunction
(define statement
  (lambda (stmt stack)
    (cond
      ((null? stmt) stack)                                                                            ; null case
      ((eq? 'throw (car stmt)) (cons (list 'throw (identify (cadr stmt) stack))  stack))                                                     ; throw call
      ((eq? 'continue (car stmt)) (cons 'continue stack))                                             ; continue call
      ((eq? 'break (car stmt)) (cons 'break stack))                                                   ; break call
      ((eq? 'funcall (car stmt)) (popReturn (runFunction (cadr stmt) (cddr stmt) stack)))             ; function call
      ((eq? 'function (car stmt)) (funcFilter (list stmt) stack))                                     ; inner function create
      ((eq? 'while (car stmt)) (while (cadr stmt) (cadr (cdr stmt)) stack))                           ; while function call
      ((eq? 'if (car stmt)) (if (eq? 4 (length stmt))
                                (ifStmt (cadr stmt) (caddr stmt) (cadddr stmt) stack)  ; if-then-else function call
                                (ifStmt (cadr stmt) (cadr (cdr stmt)) '() stack)))                    ; if-then function call
      ((eq? 'begin (car stmt)) (begin (cdr stmt) stack))                                              ; begin
      ((eq? 'try (car stmt)) (tcf (cdr stmt) stack))                                                  ; try call
      (else (varFunction stmt stack)))))                                                              ; send to varFunction

;-------------------------------------------------------------------------------------------------
;
; Arithmetic Functions (written by Kavan)
;
;-------------------------------------------------------------------------------------------------

; valid operator checker (for return statement) from the class notes
(define valid-op
  (lambda (op)
    (or (eq? op '+)          ; addition
        (eq? op '-)          ; subtraction
        (eq? op '*)          ; multiplication
        (eq? op '/)          ; division
        (eq? op '%))))       ; modulus

;runs arithmetic operation based on sign
(define identify
  (lambda (stmt stack)  ; car stmt is op
    (cond
      ((or (null? stmt)) '() )
      ((atom? stmt) (check stmt stack))
      ((valid-op (car stmt))
       (cond
         ((eq? (car stmt) '-)
          (if (eq? 2 (length stmt)) (evaluate '- 0 (check (cadr stmt) stack))
              (evaluate '- (check (cadr stmt) stack) (check (caddr stmt) stack))))
         ((eq? 2 (length stmt)) (evaluate (car stmt) (check (cadr stmt) stack) 0))
         (else (evaluate (car stmt) (check (cadr stmt) stack) (check (caddr stmt) stack)))))
      (else (check stmt stack)))))

(define evaluate
  (lambda (op val1 val2)
    (cond
      ((list? val1) val1)
      ((list? val2) val2)
      ((eq? '+ op) (+ val1 val2))         ; + op
      ((eq? '- op) (- val1 val2))         ; - op (or i guess negative sign)
      ((eq? '* op) (* val1 val2))         ; * op
      ((eq? '/ op) (/ val1 val2))         ; / op
      ((eq? '% op) (modulo val1 val2))))) ; % op

;checks if the substatement is an atom or not
(define check
  (lambda (x stack)
    (cond
      ((list? x) (if (eq? 'funcall (car x))
                     (getReturn (runFunction (cadr x) (cddr x) stack))
                     (identify x stack)))                               ; list, just do another identify call
      ((number? x) x)                                              ; number, return the number
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
    (or (eq? op '&&)                                             ; AND
        (eq? op '||)                                             ; OR
        (eq? op '==)                                             ; EQUIVALENT
        (eq? op '!=)                                             ; NOT EQUAL
        (eq? op '>)                                              ; GREATER THAN
        (eq? op '>=)                                             ; GREATER THAN OR EQUAL TO
        (eq? op '<)                                              ; LESS THAN
        (eq? op '<=)                                             ; LESS THAN OR EQUAL TO 
        (eq? op '!))))                                           ; NOT 

;identifys out boolean comparator operations
(define compound
  (lambda (stmt stack)
    (cond
      ((atom? stmt) (if (eq? stmt 'true) #t (if (eq? stmt 'false) #f)))                                    ; if stmt atom evaluate if true or false
      ((eq? (car stmt) #f) #f)                                                                             ; if stmt false return false
      ((eq? (car stmt) #t) #t)                                                                             ; if stmt true return true
      ((atom? stmt) (getValue (cadr stack) (getIndex (car stack) stmt)))                                   ; if stmt atom and not true or false evaluate
      ((eq? (car stmt) '&&) (and (compound (cadr stmt) stack) (compound (cadr (cdr stmt)) stack)))         ; if stmt AND evaluate and of recursive call
      ((eq? (car stmt) '||) (or  (compound (cadr stmt) stack) (compound (cadr (cdr stmt)) stack)))         ; if stmt OR evaluate or of recursive call
      ((eq? (car stmt) 'funcall) (runFunction (cadr stmt) (cddr stmt) stack))
      (else (simple stmt stack)))))   

;identifys out int compator operations
(define simple
  (lambda (stmt stack)
    (cond
      ((eq? (car stmt) '==) (eq? (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))         ; if stmt EQUIVALENT evaluate eq of recursive call
      ((eq? (car stmt) '!=) (not (eq? (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack))))   ; if stmt NOT EQUAL evaluate eq of recursive call
      ((eq? (car stmt) '>)  (>   (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))         ; if stmt GREATER THAN evaluate eq of recursive call
      ((eq? (car stmt) '<)  (<   (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))         ; if stmt LESS THAN evaluate eq of recursive call
      ((eq? (car stmt) '>=) (>=  (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))         ; if stmt GREATER THAN OR EQUAL TO evaluate eq of recursive call
      ((eq? (car stmt) '<=) (<=  (identify (cadr stmt) stack) (identify (cadr (cdr stmt)) stack)))         ; if stmt LESS THAN OR EQUAL TO evaluate eq of recursive call
      ((eq? (car stmt) '!)  (not (compound (cadr stmt) stack)))                                            ; if stmt NOT evaluate eq of recursive call
      (else (error "invalid boolean operation")))))                                                        ; else error case