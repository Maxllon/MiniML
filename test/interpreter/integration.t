Basic arithmetic expressions

  $ echo '2+2' | dune exec miniML
  miniML REPL
  > 4
  > 
  Goodbye!

Binary operators with precedence

  $ echo '2+3*4' | dune exec miniML
  miniML REPL
  > 14
  > 
  Goodbye!

Parentheses change precedence

  $ echo '(2+3)*4' | dune exec miniML
  miniML REPL
  > 20
  > 
  Goodbye!

Let expressions

  $ echo 'let x=5 in x+1' | dune exec miniML
  miniML REPL
  > 6
  > 
  Goodbye!

Let with arithmetic

  $ echo 'let x=10 in let y=5 in x-y' | dune exec miniML
  miniML REPL
  > 5
  > 
  Goodbye!

Let with multiplication

  $ echo 'let x=3 in let y=4 in x*y' | dune exec miniML
  miniML REPL
  > 12
  > 
  Goodbye!

Let with syntactic sugar (single argument)

  $ echo 'let f x = x+1 in f 5' | dune exec miniML
  miniML REPL
  > 6
  > 
  Goodbye!

Let rec with syntactic sugar - factorial using lambda-wrapped if for strict CBV

  $ echo 'let rec fact n = (if n=0 then \x.1 else \x.n*fact(n-1)) 0 in fact 5' | dune exec miniML
  miniML REPL
  > 120
  > 
  Goodbye!

Lambda expressions - application

  $ echo '(\x.x*2) 3' | dune exec miniML
  miniML REPL
  > 6
  > 
  Goodbye!

Lambda with multiple arguments

  $ echo '(\x y.x+y) 10 5' | dune exec miniML
  miniML REPL
  > 15
  > 
  Goodbye!

Lambda without application (prints compiled de Bruijn term)

  $ echo '\x.x' | dune exec miniML
  miniML REPL
  > (λ.i0)
  > 
  Goodbye!

Lambda with arithmetic body (no parens - application binds tighter)

  $ echo '\x.x+1 5' | dune exec miniML
  miniML REPL
  > (λ.((+ i0) (1 5)))
  > 
  Goodbye!

Unary operators - lexer error for '--'

  $ echo '--5' | dune exec miniML
  miniML REPL
  > Lexer error: 0
  > 
  Goodbye!

Unary negation

  $ echo '-(-5)' | dune exec miniML
  miniML REPL
  > 5
  > 
  Goodbye!

Unary negation with let

  $ echo 'let x=5 in -x' | dune exec miniML
  miniML REPL
  > -5
  > 
  Goodbye!

Boolean expressions

  $ echo 'true and false or true' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Boolean and (Church encoding)

  $ echo 'true and false' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0))
  > 
  Goodbye!

Boolean or (Church encoding)

  $ echo 'false or false' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0))
  > 
  Goodbye!

Comparison operators (evaluate to Church booleans)

  $ echo '2 < 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Less than or equal

  $ echo '3 <= 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Greater than

  $ echo '5 > 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Greater than or equal

  $ echo '3 >= 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Equality

  $ echo '5 = 5' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Inequality

  $ echo '5 != 5' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0))
  > 
  Goodbye!

If then else - true branch

  $ echo 'if true then 1 else 2' | dune exec miniML
  miniML REPL
  > 1
  > 
  Goodbye!

If then else - false branch

  $ echo 'if false then 1 else 2' | dune exec miniML
  miniML REPL
  > 2
  > 
  Goodbye!

If with comparison (now evaluates correctly under strict CBV)

  $ echo 'if 5 > 3 then 10 else 20' | dune exec miniML
  miniML REPL
  > 10
  > 
  Goodbye!

Division

  $ echo '10 / 2' | dune exec miniML
  miniML REPL
  > 5
  > 
  Goodbye!

Division with let

  $ echo 'let x=20 in x/4' | dune exec miniML
  miniML REPL
  > 5
  > 
  Goodbye!

Xor operator

  $ echo 'true xor false' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1))
  > 
  Goodbye!

Xor both true

  $ echo 'true xor true' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0))
  > 
  Goodbye!

Parser error

  $ echo '1+' | dune exec miniML
  miniML REPL
  > Parser error: Error in parse_atom
  > 
  Goodbye!

Lexer error (number too long)

  $ echo '12345678901234567890' | dune exec miniML
  miniML REPL
  > Lexer error: 0
  > 
  Goodbye!

Division by zero (stays unevaluated)

  $ echo '1 / 0' | dune exec miniML
  miniML REPL
  > ((/ 1) 0)
  > 
  Goodbye!

Unbound variable (stays as free variable)

  $ echo 'x' | dune exec miniML
  miniML REPL
  > x
  > 
  Goodbye!

Let rec with unused recursive variable

  $ echo 'let rec x = x in 1' | dune exec miniML
  miniML REPL
  > 1
  > 
  Goodbye!

Capture-avoiding substitution (fixed: now correctly gives z, not 0)

  $ echo '(\x z.x) z 0' | dune exec miniML
  miniML REPL
  > z
  > 
  Goodbye!
