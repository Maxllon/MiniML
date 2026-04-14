Basic arithmetic expressions

  $ echo '2+2' | dune exec miniML
  miniML REPL - type :h to list commands
  > 4
  > 
  Goodbye!

Binary operators with precedence

  $ echo '2+3*4' | dune exec miniML
  miniML REPL - type :h to list commands
  > 14
  > 
  Goodbye!

Parentheses change precedence

  $ echo '(2+3)*4' | dune exec miniML
  miniML REPL - type :h to list commands
  > 20
  > 
  Goodbye!

Let expressions

  $ echo 'let x=5 in x+1' | dune exec miniML
  miniML REPL - type :h to list commands
  > 6
  > 
  Goodbye!

Let with arithmetic

  $ echo 'let x=10 in let y=5 in x-y' | dune exec miniML
  miniML REPL - type :h to list commands
  > 5
  > 
  Goodbye!

Let with multiplication

  $ echo 'let x=3 in let y=4 in x*y' | dune exec miniML
  miniML REPL - type :h to list commands
  > 12
  > 
  Goodbye!

Lambda expressions

  $ echo '\x -> x+1 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > <fun x>
  > 
  Goodbye!

Lambda with body

  $ echo '(\x -> x*2) 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > 6
  > 
  Goodbye!

Lambda with addition

  $ echo '(\x y -> x+y) 10 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > 15
  > 
  Goodbye!

Unary operators

  $ echo '--5' | dune exec miniML
  miniML REPL - type :h to list commands
  > Error: Lexer error: 0
  > 
  Goodbye!

Unary negation

  $ echo '-(-5)' | dune exec miniML
  miniML REPL - type :h to list commands
  > 5
  > 
  Goodbye!

Unary negation with let

  $ echo 'let x=5 in -x' | dune exec miniML
  miniML REPL - type :h to list commands
  > -5
  > 
  Goodbye!

Boolean expressions

  $ echo 'true and false or true' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Boolean and

  $ echo 'true and false' | dune exec miniML
  miniML REPL - type :h to list commands
  > false
  > 
  Goodbye!

Boolean or

  $ echo 'false or false' | dune exec miniML
  miniML REPL - type :h to list commands
  > false
  > 
  Goodbye!

Comparison operators

  $ echo '2 < 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Less than or equal

  $ echo '3 <= 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Greater than

  $ echo '5 > 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Greater than or equal

  $ echo '3 >= 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Equality

  $ echo '5 = 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Inequality

  $ echo '5 != 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > false
  > 
  Goodbye!

If then else - true branch

  $ echo 'if true then 1 else 2' | dune exec miniML
  miniML REPL - type :h to list commands
  > 1
  > 
  Goodbye!

If then else - false branch

  $ echo 'if false then 1 else 2' | dune exec miniML
  miniML REPL - type :h to list commands
  > 2
  > 
  Goodbye!

If with comparison

  $ echo 'if 5 > 3 then 10 else 20' | dune exec miniML
  miniML REPL - type :h to list commands
  > 10
  > 
  Goodbye!

Let rec - recursive function

  $ echo 'let rec fact = \n -> if n = 0 then 1 else n * (fact (n - 1)) in fact 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > 120
  > 
  Goodbye!

Let rec fibonacci

  $ echo 'let rec fib = \n -> if n = 0 then 0 else if n = 1 then 1 else (fib (n-1)) + (fib (n-2)) in fib 10' | dune exec miniML
  miniML REPL - type :h to list commands
  > 55
  > 
  Goodbye!

Division

  $ echo '10 / 2' | dune exec miniML
  miniML REPL - type :h to list commands
  > 5
  > 
  Goodbye!

Division with let

  $ echo 'let x=20 in x/4' | dune exec miniML
  miniML REPL - type :h to list commands
  > 5
  > 
  Goodbye!

Xor operator

  $ echo 'true xor false' | dune exec miniML
  miniML REPL - type :h to list commands
  > true
  > 
  Goodbye!

Xor both true

  $ echo 'true xor true' | dune exec miniML
  miniML REPL - type :h to list commands
  > false
  > 
  Goodbye!

Help command

  $ echo ':h' | dune exec miniML
  miniML REPL - type :h to list commands
  > Available commands:
    :h, :help   - show this help
    :l <file>  - load and evaluate file
    :t         - show defined values
    :c         - clear screen
    :q, :quit  - exit REPL
  > 
  Goodbye!

Exit command

  $ echo ':q' | dune exec miniML
  miniML REPL - type :h to list commands
  > Goodbye!

Error handling

  $ echo '1+' | dune exec miniML
  miniML REPL - type :h to list commands
  > Error: Error in parse_atom
  > 
  Goodbye!

Lexer error

  $ echo '12345678901234567890' | dune exec miniML
  miniML REPL - type :h to list commands
  > Error: Lexer error: 0
  > 
  Goodbye!

Division by zero

  $ echo '1 / 0' | dune exec miniML
  miniML REPL - type :h to list commands
  > Error: Division by zero
  > 
  Goodbye!

Unbound variable

  $ echo 'x' | dune exec miniML
  miniML REPL - type :h to list commands
  > Error: Unbound variable: x
  > 
  Goodbye!
