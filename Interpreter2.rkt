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
      ((trueFalse? tfStmt var val) (statement stmt1 var val))
      (else (statement stmt2 var val)))))

(define while
  (lambda (tfStmt body var val)
    (cond
      ((trueFalse? tfStmt var val) (while tfStmt body (lambda ((statement body var val))))))));; not complete
 

(define varFunction
  (lambda (stmt var val)
    (cond
      ((eq? 'var (car stmt)) (declare (cadr stmt) var val))
      ((eq? '= (car stmt)) (assign (cadr stmt) 

       
(define instr
  (lambda (l var val)
    (cond
      ((null? l) #f)
      ((exists? var 'return) (getValue val (getIndex var 'return)))
      (else (begin
              (statement (car l) var val)
              (instr (cdr l) var val))))))