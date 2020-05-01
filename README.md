# Short description
- This is implementation of tiny Lisp/Scheme interpreter.
- It can load file during running repl, just give path to the file with lisp program as an argument.
- Main file is Repl. It should be run with or without args and after you will see place for entering your commands.

# Commands and examples:
   - *(atom a) -> t* There are two atoms Number and Symbol. t means true, it is Symbol
   - *(atom 10) -> t*
   - *(atom (q (1 2 3))) -> ()* Rest is lists. () means false
   - *(q a) -> a*. This is **quote** command, result is atom *a*
   - *(q (a b c)) -> (a b c)*. This is **quote** command, result is list *(a b c)*
   - *(car (q (a b c))) -> a* This is **car** command, result is atom *a*
   - *(cdr (q (a b c))) -> (b c)* This is **cdr** command, result is list *(b c)*
   - *(cond ((eq (q a) (q b)) (q first)) ((atom (q a)) (q second))) -> second* This is **cond** (condition) command, result is atom *second*
   - *(+ 1 2 (* 2 3) (/ 1 2))* Example of arithmetic operations. Interpreter is used as calculator.
   - *(< 1 2)* Boolean operation on numbers
   - *(lambda (x) (+ x 2))* Creates anonymus function
   - *((lambda (x) (+ x 2)) 1)* Executes anonymus function
   - *(define x 1)* Defines a variable
   - *(set! x 2)* Changes value of variable
   - *(define plustwo (lambda (x) (+ x 2)))* Defines a new function with name plustwo
   - *(plustwo 12)* Executing function
   - *(begin (+ 1 2) (define x 2) (/ x 100))* Allows to combine few lisp expressions, returns last evaluated

You can find examples of some declared functions in files within examples directory. This functions can be uploaded to environment of your current execution and you can use them.
For example in examples/area.lisp you can find function to compute circle area and you can use it in following way *(area 1)*. From small set of functions and defining new ones you can create complex programs.
Also you can find file **jmc.ps** in main directory. This is a cool article about Lisp origins written by cool computer scientist Paul Graham. It is good reference for knowing Lisp better.  
