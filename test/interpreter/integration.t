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
  > λx.+ x (1 5)
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
  > λx.λy.x
  > 
  Goodbye!

Boolean and

  $ echo 'true and false' | dune exec miniML
  miniML REPL - type :h to list commands
  > λx.λy.y
  > 
  Goodbye!

Boolean or

  $ echo 'false or false' | dune exec miniML
  miniML REPL - type :h to list commands
  > λx.λy.y
  > 
  Goodbye!

Comparison operators

  $ echo '2 < 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > < 2 3
  > 
  Goodbye!

Less than or equal

  $ echo '3 <= 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > <= 3 3
  > 
  Goodbye!

Greater than

  $ echo '5 > 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > > 5 3
  > 
  Goodbye!

Greater than or equal

  $ echo '3 >= 3' | dune exec miniML
  miniML REPL - type :h to list commands
  > >= 3 3
  > 
  Goodbye!

Equality

  $ echo '5 = 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > λx.λy.x
  > 
  Goodbye!

Inequality

  $ echo '5 != 5' | dune exec miniML
  miniML REPL - type :h to list commands
  > λx.λy.y
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
  > > 5 3 (λ$.10) (λ$.20) 0
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
  > λx.λy.x
  > 
  Goodbye!

Xor both true

  $ echo 'true xor true' | dune exec miniML
  miniML REPL - type :h to list commands
  > λx.λy.y
  > 
  Goodbye!

Help command

  $ echo ':h' | dune exec miniML
  miniML REPL - type :h to list commands
  > Available commands:
    :h, :help      - show this help
    :load <code>   - load code for step-by-step reduction
    :next          - perform one step of reduction
    :t             - show defined values
    :c             - clear screen
    :compile <code> - compile code to lambda and print it
    :q, :quit      - exit REPL
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
  > / 1 0
  > 
  Goodbye!

Unbound variable

  $ echo 'x' | dune exec miniML
  miniML REPL - type :h to list commands
  > x
  > 
  Goodbye!

Let rec with unused recursive variable

  $ echo 'let rec x = x in 1' | dune exec miniML
  miniML REPL - type :h to list commands
  > 1
  > 
  Goodbye!

Identity function

  $ echo '\x -> x' | dune exec miniML
  miniML REPL - type :h to list commands
  > λx.x
  > 
  Goodbye!

Next without load

  $ printf ':next\n:q\n' | dune exec miniML
  miniML REPL - type :h to list commands
  > Nothing loaded. Use :load <code> first.
  > Goodbye!

Load and step reduction

  $ printf ':load let x = 3 + 2 in x\n:next\n:next\n:next\n:q\n' | dune exec miniML
  miniML REPL - type :h to list commands
  > Step 0: (λx.x) (+ 3 2)
  > Step 1: (λx.x) 5
  > Step 2: 5
  > Step 2: 5
  Nothing more to reduce.
  > Goodbye!
