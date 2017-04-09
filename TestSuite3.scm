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

(check-not-exn
         (lambda () (interpret (parser "Test3/Test4.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test4.txt")) 55)) 'Test4_Fail))



;Test5
(check-not-exn
        (lambda () (interpret (parser "Test3/Test5.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test5.txt")) 1)) 'Test5_Fail))




;Test6
(check-not-exn
        (lambda () (interpret (parser "Test3/Test6.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test6.txt")) 115)) 'Test6_Fail))


;Test7
(check-not-exn
         (lambda () (interpret (parser "Test3/Test7.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test7.txt")) 'true)) 'Test7_Fail))

;Test8
(check-not-exn
         (lambda () (interpret (parser "Test3/Test8.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test8.txt")) 20)) 'Test8_Fail))

;Test9
(check-not-exn
         (lambda () (interpret (parser "Test3/Test9.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test9.txt")) 24)) 'Test9_Fail))

;Test10
(check-not-exn
         (lambda () (interpret (parser "Test3/Test10.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test10.txt")) 2)) 'Test10_Fail))

;Test11
(check-not-exn
         (lambda () (interpret (parser "Test3/Test11.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test11.txt")) 35)) 'Test11_Fail))

;(if (not (check-exn
;          exn:fail?
;          (lambda () (interpret (parser "Test3/Test11.txt"))))) 'Test11_Fail)


;Test12
'Test12
(with-handlers ([exn:fail? displayln])
    (if (atom?(interpret (parser "Test3/Test12.txt"))) 'Test12_Fail))
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test3/Test12.txt"))))) 'Test12_Fail)



;Test13

(check-not-exn
         (lambda () (interpret (parser "Test3/Test13.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test13.txt")) 90)) 'Test13_Fail))

;Test14
(check-not-exn
         (lambda () (interpret (parser "Test3/Test14.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test14.txt")) 69)) 'Test14_Fail))


;Test15
(check-not-exn
         (lambda () (interpret (parser "Test3/Test15.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test15.txt")) 87)) 'Test15_Fail))

;Test16
(check-not-exn
         (lambda () (interpret (parser "Test3/Test16.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test16.txt")) 64)) 'Test16_Fail))
                                                        
      

;Test17
'Test17
(with-handlers ([exn:fail? displayln])
    (if (atom?(interpret (parser "Test3/Test17.txt"))) 'Test17_Fail))
(if (not (check-exn
          exn:fail?
          (lambda () (interpret (parser "Test3/Test17.txt"))))) 'Test17_Fail)

;Test18
(check-not-exn
         (lambda () (interpret (parser "Test3/Test18.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test18.txt")) 125)) 'Test18_Fail))

;Test19
(check-not-exn
         (lambda () (interpret (parser "Test3/Test19.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test19.txt")) 100)) 'Test19_Fail))

;Test20
(check-not-exn
         (lambda () (interpret (parser "Test3/Test20.txt"))))
(with-handlers ([exn:fail? displayln])
    (if (not(eq?(interpret (parser "Test3/Test20.txt")) 2000400)) 'Test20_Fail))