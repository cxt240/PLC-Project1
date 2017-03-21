# PLC-Project1

@authors: Kavan Mally, Chris Tsuei, Andrew Su

Written in Scheme

We are not implementing multiple assigns in this interpreter.

Interpreter is the file containing the compiler.
To run interpreter given file (filename.txt), call the function run like this:

(interpreter (parser "filename.txt"))

To test all files, just run TestSuite.scm file

Features:

* loose data type
* variable assignment
* while iterator
* basic arithmetic
* boolean operations
* blocks {}
* goto's such as break and return
* error handling (try/catch/finally)
