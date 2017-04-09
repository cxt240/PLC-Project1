# PLC-Project2

@authors: Kavan Mally, Chris Tsuei, Andrew Su

Written in Scheme

We are not implementing multiple assigns in this interpreter.

Interpreter is the file containing the compiler.
To run interpreter given file (filename.txt), call the function run like this:

(interpret (parser "filename.txt"))

To test all files, just run TestSuite3.scm file
TestSuites 1 and 2 no longer apply to the project. To use those test suites, open the .zip files containing part 2 and run those tests using that version of the interpreter.

Features:

* loose data type
* variable assignment
* while iterator
* basic arithmetic
* boolean operations
* blocks {}
* goto's such as break and return
* error handling (try/catch/finally)
