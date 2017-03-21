(load "interpreter.scm")

(require rackunit)



;Interpreter Part 1

;Test1 
(if (not (check-eq? (interpreter (parser "Test1/Test1.txt")) 150 "Test1 Fail")) 'Test1_Fail)

;Test2
(check-eq? (interpreter (parser "Test1/Test2.txt")) -4 "Test2 Fail")

;Test3
(if (not(eq?(interpreter (parser "Test1/Test3.txt")) 10)) 'Test3_Fail)

;Test4
(if (not(eq?(interpreter (parser "Test1/Test4.txt")) 16)) 'Test4_Fail)

;Test5
(if (not(eq?(interpreter (parser "Test1/Test5.txt")) 220)) 'Test5_Fail)

;Test6
(if (not(eq?(interpreter (parser "Test1/Test6.txt")) 5)) 'Test6_Fail)


;Test7
(if (not(eq?(interpreter (parser "Test1/Test7.txt")) 6)) 'Test7_Fail)

;Test8
(if (not(eq?(interpreter (parser "Test1/Test8.txt")) 10)) 'Test8_Fail)

;Test9
(if (not(eq?(interpreter (parser "Test1/Test9.txt")) 5)) 'Test9_Fail)

;Test10
(if (not(eq?(interpreter (parser "Test1/Test10.txt")) -39)) 'Test10_Fail)

;Test11
(if (not (check-exn
          exn:fail?
          (lambda () (interpreter (parser "Test1/Test11.txt"))))) 'Test11_Fail)


;Test12
;(if (test/exn (interpreter (parser "Test1/Test12.txt")) "declaring") '() )
(if (not (check-exn
          exn:fail?
          (lambda () (interpreter (parser "Test1/Test12.txt"))))) 'Test12_Fail)

;Test13
(if (not (check-exn
          exn:fail?
          (lambda () (interpreter (parser "Test1/Test13.txt"))))) 'Test13_Fail)

;Test14
(if (not (check-exn
          exn:fail?
          (lambda () (interpreter (parser "Test1/Test13.txt"))))) 'Test13_Fail)


;Test15
(check-eq? (interpreter (parser "Test1/Test15.txt")) 'true "Test15 Fail")

;Test16
(check-eq? (interpreter (parser "Test1/Test16.txt")) 100 "Test16 Fail")

;Test17
(check-eq? (interpreter (parser "Test1/Test17.txt")) 'false "Test17 Fail")

;Test18
(check-eq? (interpreter (parser "Test1/Test18.txt")) 'true "Test18 Fail")

;Test19
(check-eq? (interpreter (parser "Test1/Test19.txt")) 128 "Test19 Fail")

;Test20
(check-eq? (interpreter (parser "Test1/Test20.txt")) 12 "Test20 Fail")
