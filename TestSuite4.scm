(load "Interpret.scm")

(require rackunit)


; Project 4

;Test 1
(if (not(eq? (interpret "Test4/Test1.txt" 'A) 15)) 'Test1_Fail)

;Test 2
(if (not(eq? (interpret "Test4/Test2.txt" 'A) 12)) 'Test2_Fail)

;Test 3
(if (not(eq? (interpret "Test4/Test3.txt" 'A) 125)) 'Test3_Fail)

;Test 4
(if (not(eq? (interpret "Test4/Test4.txt" 'A) 36)) 'Test4_Fail)

;Test 5
(if (not(eq? (interpret "Test4/Test5.txt" 'A) 54)) 'Test5_Fail)

;Test 6
(if (not(eq? (interpret "Test4/Test6.txt" 'A) 110)) 'Test6_Fail)

;Test 7
(if (not(eq? (interpret "Test4/Test7.txt" 'C) 26)) 'Test7_Fail)

;Test 8
(if (not(eq? (interpret "Test4/Test8.txt" 'Square) 117)) 'Test8_Fail)

;Test 9
(if (not(eq? (interpret "Test4/Test9.txt" 'Square) 32)) 'Test9_Fail)

;Test 10
(if (not(eq? (interpret "Test4/Test10.txt" 'List) 15)) 'Test10_Fail)

;Test 11
(if (not(eq? (interpret "Test4/Test11.txt" 'List) 123456)) 'Test11_Fail)

;Test 12
(if (not(eq? (interpret "Test4/Test12.txt" 'List) 5285)) 'Test12_Fail)

;Test 13
(if (not(eq? (interpret "Test4/Test13.txt" 'C) -716)) 'Test13_Fail)

