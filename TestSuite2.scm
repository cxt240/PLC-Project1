(load "interpreter.scm")

(require rackunit)



;Project 2

;Test1 
(if (not(eq? (interpret (parser "Test2/Test1.txt")) 20)) 'Test1_Fail)

;Test2
(if (not(eq?(interpret (parser "Test2/Test2.txt")) 164)) 'Test2_Fail)

;Test3
(if (not(eq?(interpret (parser "Test2/Test3.txt")) 32)) 'Test3_Fail)

;Test4
(if (not(eq?(interpret (parser "Test2/Test4.txt")) 2)) 'Test4_Fail)

;Test5
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test2/Test5.txt"))))) 'Test5_Fail)

;Test6
(if (not(eq?(interpret (parser "Test2/Test6.txt")) 25)) 'Test6_Fail)


;Test7
(if (not(eq?(interpret (parser "Test2/Test7.txt")) 21)) 'Test7_Fail)

;Test8
(if (not(eq?(interpret (parser "Test2/Test8.txt")) 6)) 'Test8_Fail)

;Test9
(if (not(eq?(interpret (parser "Test2/Test9.txt")) -1)) 'Test9_Fail)

;Test10
(if (not(eq?(interpret (parser "Test2/Test10.txt")) 789)) 'Test10_Fail)

;Test11
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test2/Test11.txt"))))) 'Test11_Fail)


;Test12
;(if (test/exn (interpret (parser "Test2/Test12.txt")) "declaring") '() )
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test2/Test12.txt"))))) 'Test12_Fail)

;Test13
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test2/Test13.txt"))))) 'Test13_Fail)

;Test14
(if (not(eq?(interpret (parser "Test2/Test14.txt")) 12)) 'Test14_Fail)


;Test15
(if (check-not-exn
         (lambda () (interpret (parser "Test2/Test15.txt")))) 
         (if (not(eq?(interpret (parser "Test2/Test15.txt")) 125)) 'Test15_Fail)
         'Test15_Fail_With_Error) ;else statement

;Test16
(if (check-not-exn
         (lambda () (interpret (parser "Test2/Test16.txt")))) 
         (if (not(eq?(interpret (parser "Test2/Test16.txt")) 110)) 'Test16_Fail)
         'Test16_Fail_With_Error) ;else statement

(cond
  ((check-not-exn
         (lambda () (interpret (parser "Test2/Test14.txt")))) 'CheckIsTrue)
  (else 'CheckIsFalse))
                                                        
      

;Test17
(if (check-not-exn
         (lambda () (interpret (parser "Test2/Test17.txt"))))
         (if (not(eq?(interpret (parser "Test2/Test17.txt")) 2000400)) 'Test17_Fail)
         'Test17_Fail_With_Error) ;else statement

;Test18
(if (check-not-exn
         (lambda () (interpret (parser "Test2/Test18.txt"))))
          (if (not(eq?(interpret (parser "Test2/Test18.txt")) 101)) 'Test18_Fail)
           'Test18_Fail_With_Error) ;else statement

;Test19
(check-exn
 exn:fail?
 (lambda () (interpret (parser "Test2/Test19.txt"))))