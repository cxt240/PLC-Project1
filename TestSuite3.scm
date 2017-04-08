(load "Interpret.scm")

(require rackunit)



;Project 3

;Test1 
(if (not(eq? (interpret (parser "Test3/Test1.txt")) 10)) 'Test1_Fail)

;Test2
(if (not(eq?(interpret (parser "Test3/Test2.txt")) 14)) 'Test2_Fail)

;Test3
(if (not(eq?(interpret (parser "Test3/Test3.txt")) 45)) 'Test3_Fail)

;Test4
(with-handlers ([exn:fail? (lambda (v) 'Test4_Fail)])
    (if (not(eq?(interpret (parser "Test3/Test4.txt")) 55)) 'Test4_Fail))

(check-not-exn
         (lambda () (interpret (parser "Test3/Test4.txt"))))



;Test5
(with-handlers ([exn:fail? (lambda (v) 'Test5_Fail)])
    (if (not(eq?(interpret (parser "Test3/Test5.txt")) 1)) 'Test5_Fail))
(check-not-exn
        (lambda () (interpret (parser "Test3/Test5.txt"))))



;(if (not (check-exn
 ;         exn:fail?
 ;         (lambda () (interpret (parser "Test3/Test5.txt"))))) 'Test5_Fail)

;Test6
(with-handlers ([exn:fail? (lambda (v) 'Test6_Fail)])
    (if (not(eq?(interpret (parser "Test3/Test6.txt")) 115)) 'Test6_Fail))
(check-not-exn
        (lambda () (interpret (parser "Test3/Test6.txt"))))


;Test7
(with-handlers ([exn:fail? (lambda (v) 'Test7_Error)])
    (if (not(eq?(interpret (parser "Test3/Test7.txt")) 'true)) 'Test7_Fail))
(check-not-exn
         (lambda () (interpret (parser "Test3/Test7.txt"))))

;Test8
(with-handlers ([exn:fail? (lambda (v) 'Test8_Error)])
    (if (not(eq?(interpret (parser "Test3/Test8.txt")) 20)) 'Test8_Fail))
(with-handlers ([exn:fail? (lambda (exn) +inf.0)])
    (if (not(eq?(interpret (parser "Test3/Test8.txt")) 20)) '()))

;Test9
(with-handlers ([exn:fail? (lambda (v) 'Test9_Error)])
    (if (not(eq?(interpret (parser "Test3/Test9.txt")) 24)) 'Test9_Fail))
(with-handlers ([exn:fail? (lambda (exn) +inf.0)])
    (if (not(eq?(interpret (parser "Test3/Test9.txt")) 24)) '()))

;Test10
(with-handlers ([exn:fail? (lambda (v) 'Test10_Error)])
    (if (not(eq?(interpret (parser "Test3/Test10.txt")) 2)) 'Test10_Fail))
(with-handlers ([exn:fail? (lambda (exn) +inf.0)])
    (if (not(eq?(interpret (parser "Test3/Test10.txt")) 2)) 'Test10_Fail))

;Test11
(with-handlers ([exn:fail? (lambda (v) 'Test11_Error)])
    (if (not(eq?(interpret (parser "Test3/Test11.txt")) 35)) 'Test11_Fail))
(with-handlers ([exn:fail? (lambda (exn) +inf.0)])
    (if (not(eq?(interpret (parser "Test3/Test11.txt")) 35)) 'Test11_Fail))

;(if (not (check-exn
;          exn:fail?
;          (lambda () (interpret (parser "Test3/Test11.txt"))))) 'Test11_Fail)


;Test12
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test3/Test12.txt"))))) 'Test12_Fail)

;Test13


(with-handlers ([exn:fail? (lambda (v) 'Test13_Error)])
    (if (not(eq?(interpret (parser "Test3/Test11.txt")) 90)) 'Test13_Fail))
(with-handlers ([exn:fail? (lambda (exn) +inf.0)])
    (if (not(eq?(interpret (parser "Test3/Test11.txt")) 90)) 'Test13_Fail))

;Test14
(if (not(eq?(interpret (parser "Test3/Test14.txt")) 69)) 'Test14_Fail)


;Test15
(if (check-not-exn
         (lambda () (interpret (parser "Test3/Test15.txt")))) 
         (if (not(eq?(interpret (parser "Test3/Test15.txt")) 87)) 'Test15_Fail)
         'Test15_Fail_With_Error) ;else statement

;Test16
(if (check-not-exn
         (lambda () (interpret (parser "Test3/Test16.txt")))) 
         (if (not(eq?(interpret (parser "Test3/Test16.txt")) 64)) 'Test16_Fail)
         'Test16_Fail_With_Error) ;else statement
                                                        
      

;Test17
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test3/Test17.txt"))))) 'Test17_Fail) ;else statement

;Test18
(if (check-not-exn
         (lambda () (interpret (parser "Test3/Test18.txt"))))
          (if (not(eq?(interpret (parser "Test3/Test18.txt")) 125)) 'Test18_Fail)
           'Test18_Fail_With_Error) ;else statement

;Test19
(if (check-not-exn
         (lambda () (interpret (parser "Test3/Test19.txt"))))
          (if (not(eq?(interpret (parser "Test3/Test19.txt")) 100)) 'Test19_Fail)
           'Test18_Fail_With_Error)

;Test20
(if (check-not-exn
         (lambda () (interpret (parser "Test2/Test18.txt"))))
          (if (not(eq?(interpret (parser "Test2/Test18.txt")) 2000400)) 'Test18_Fail)
           'Test18_Fail_With_Error)