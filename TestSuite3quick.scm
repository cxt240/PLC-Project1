(load "Interpret.scm")

(require rackunit)



;Project 3

;copy to display what errors occured if any
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test1.txt")) 10)) 'Test1_Fail))

;copy to display what value is wrong if Failing with incorrect output
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test1.txt")) 10)) (cons 'Test1_Result (interpret (parser "Test3/Test1.txt")))
        'Test1_Pass)) ;else cond

;Test1 

(with-handlers ([exn:fail? (lambda (v) 'Test1_Error)])
    (if (not(eq?(interpret (parser "Test3/Test1.txt")) 10)) 'Test1_Fail))

;Test2
(with-handlers ([exn:fail? (lambda (v) 'Test2_Error)])
    (if (not(eq?(interpret (parser "Test3/Test2.txt")) 14)) 'Test2_Fail))

;Test3

(with-handlers ([exn:fail? (lambda (v) 'Test3_Error)])
    (if (not(eq?(interpret (parser "Test3/Test3.txt")) 45)) 'Test3_Fail))

;Test4
(with-handlers ([exn:fail? (lambda (v) 'Test4_Error)])
    (if (not(eq?(interpret (parser "Test3/Test4.txt")) 55)) 'Test4_Fail))


;Test5
(with-handlers ([exn:fail? (lambda (v) 'Test5_Error)])
    (if (not(eq?(interpret (parser "Test3/Test5.txt")) 1)) 'Test5_Fail))


;Test6
(with-handlers ([exn:fail? (lambda (v) 'Test6_Error)])
    (if (not(eq?(interpret (parser "Test3/Test6.txt")) 115)) 'Test6_Fail))

;Test7
(with-handlers ([exn:fail? (lambda (v) 'Test7_Error)])
    (if (not(eq?(interpret (parser "Test3/Test7.txt")) 'true)) 'Test7_Fail))

;Test8
(with-handlers ([exn:fail? (lambda (v) 'Test8_Error)])
    (if (not(eq?(interpret (parser "Test3/Test8.txt")) 20)) 'Test8_Fail))

;Test9
(with-handlers ([exn:fail? (lambda (v) 'Test9_Error)])
    (if (not(eq?(interpret (parser "Test3/Test9.txt")) 24)) 'Test9_Fail))

;Test10
(with-handlers ([exn:fail? (lambda (v) 'Test10_Error)])
    (if (not(eq?(interpret (parser "Test3/Test10.txt")) 2)) 'Test10_Fail))

;Test11
(with-handlers ([exn:fail? (lambda (v) 'Test11_Error)])
    (if (not(eq?(interpret (parser "Test3/Test11.txt")) 35)) 'Test11_Fail))

;(if (not (check-exn
;          exn:fail?
;          (lambda () (interpret (parser "Test3/Test11.txt"))))) 'Test11_Fail)


;Test12
'Test12_Error_Thrown
(with-handlers ([exn:fail? displayln])
    (if (atom?(interpret (parser "Test3/Test12.txt"))) 'Test12_Fail))


;Test13

(with-handlers ([exn:fail? (lambda (v) 'Test13_Error)])
    (if (not(eq?(interpret (parser "Test3/Test13.txt")) 90)) 'Test13_Fail))

;Test14
(with-handlers ([exn:fail? (lambda (v) 'Test14_Error)])
    (if (not(eq?(interpret (parser "Test3/Test14.txt")) 69)) 'Test14_Fail))
(interpret (parser "Test3/Test14.txt"))

;Test15
(with-handlers ([exn:fail? (lambda (v) 'Test15_Error)])
    (if (not(eq?(interpret (parser "Test3/Test15.txt")) 87)) 'Test15_Fail))

;Test16
(with-handlers ([exn:fail? (lambda (v) 'Test16_Error)])
    (if (not(eq?(interpret (parser "Test3/Test16.txt")) 64)) 'Test16_Fail))
                                                        
      

;Test17
'Test17_Error_Thrown
(with-handlers ([exn:fail? displayln])
    (if (atom?(interpret (parser "Test3/Test17.txt"))) 'Test17_Fail))

;Test18
(with-handlers ([exn:fail? (lambda (v) 'Test18_Error)])
    (if (not(eq?(interpret (parser "Test3/Test18.txt")) 125)) 'Test18_Fail))

;Test19
(with-handlers ([exn:fail? (lambda (v) 'Test19_Error)])
    (if (not(eq?(interpret (parser "Test3/Test19.txt")) 100)) 'Test19_Fail))

;Test20
(with-handlers ([exn:fail? (lambda (v) 'Test20_Error)])
    (if (not(eq?(interpret (parser "Test3/Test20.txt")) 2000400)) 'Test20_Fail))