Basic arithmetic expressions

  $ echo '2+2' | dune exec miniML
  miniML REPL
  > 4 : Int
  > 
  Goodbye!

Binary operators with precedence

  $ echo '2+3*4' | dune exec miniML
  miniML REPL
  > 14 : Int
  > 
  Goodbye!

Parentheses change precedence

  $ echo '(2+3)*4' | dune exec miniML
  miniML REPL
  > 20 : Int
  > 
  Goodbye!

Let expressions

  $ echo 'let x=5 in x+1' | dune exec miniML
  miniML REPL
  > 6 : Int
  > 
  Goodbye!

Let with arithmetic

  $ echo 'let x=10 in let y=5 in x-y' | dune exec miniML
  miniML REPL
  > 5 : Int
  > 
  Goodbye!

Let with multiplication

  $ echo 'let x=3 in let y=4 in x*y' | dune exec miniML
  miniML REPL
  > 12 : Int
  > 
  Goodbye!

Let with syntactic sugar (single argument)

  $ echo 'let f x = x+1 in f 5' | dune exec miniML
  miniML REPL
  > 6 : Int
  > 
  Goodbye!

Let rec with syntactic sugar - factorial using lambda-wrapped if for strict CBV

  $ echo 'let rec fact n = (if n=0 then \x.1 else \x.n*fact(n-1)) 0 in fact 5' | dune exec miniML
  miniML REPL
  > 120 : Int
  > 
  Goodbye!

Let rec with lif (lazy if) - factorial, both branches are lazy

  $ echo 'let rec fact = \n.lif n=0 then 1 else n*fact(n-1) in fact 5' | dune exec miniML
  miniML REPL
  > 120 : Int
  > 
  Goodbye!

Lambda expressions - application

  $ echo '(\x.x*2) 3' | dune exec miniML
  miniML REPL
  > 6 : Int
  > 
  Goodbye!

Lambda with multiple arguments

  $ echo '(\x y.x+y) 10 5' | dune exec miniML
  miniML REPL
  > 15 : Int
  > 
  Goodbye!

Lambda without application (prints compiled de Bruijn term)

  $ echo '\x.x' | dune exec miniML
  miniML REPL
  > (λ.i0) : (t0 -> t0)
  > 
  Goodbye!

Lambda with arithmetic body (no parens - application binds tighter, so type error)

  $ echo '\x.x+1 5' | dune exec miniML
  miniML REPL
  > Typecheker error: Cannot equalise:
  Int
  (Int -> t1)
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
  > 5 : Int
  > 
  Goodbye!

Unary negation with let

  $ echo 'let x=5 in -x' | dune exec miniML
  miniML REPL
  > -5 : Int
  > 
  Goodbye!

Boolean expressions

  $ echo 'true and false or true' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Boolean and (Church encoding)

  $ echo 'true and false' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0)) : Bool
  > 
  Goodbye!

Boolean or (Church encoding)

  $ echo 'false or false' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0)) : Bool
  > 
  Goodbye!

Comparison operators (evaluate to Church booleans)

  $ echo '2 < 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Less than or equal

  $ echo '3 <= 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Greater than

  $ echo '5 > 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Greater than or equal

  $ echo '3 >= 3' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Equality

  $ echo '5 = 5' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Inequality

  $ echo '5 != 5' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0)) : Bool
  > 
  Goodbye!

If then else - true branch

  $ echo 'if true then 1 else 2' | dune exec miniML
  miniML REPL
  > 1 : Int
  > 
  Goodbye!

If then else - false branch

  $ echo 'if false then 1 else 2' | dune exec miniML
  miniML REPL
  > 2 : Int
  > 
  Goodbye!

If with comparison (now evaluates correctly under strict CBV)

  $ echo 'if 5 > 3 then 10 else 20' | dune exec miniML
  miniML REPL
  > 10 : Int
  > 
  Goodbye!

Division

  $ echo '10 / 2' | dune exec miniML
  miniML REPL
  > 5 : Int
  > 
  Goodbye!

Division with let

  $ echo 'let x=20 in x/4' | dune exec miniML
  miniML REPL
  > 5 : Int
  > 
  Goodbye!

Xor operator

  $ echo 'true xor false' | dune exec miniML
  miniML REPL
  > (λ.(λ.i1)) : Bool
  > 
  Goodbye!

Xor both true

  $ echo 'true xor true' | dune exec miniML
  miniML REPL
  > (λ.(λ.i0)) : Bool
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

Division by zero (evaluates to error)

  $ echo '1 / 0' | dune exec miniML
  miniML REPL
  > error : Int
  > 
  Goodbye!

Unbound variable (typechecker rejects before evaluation)

  $ echo 'x' | dune exec miniML
  miniML REPL
  > Typecheker error: Unbound variable: "x"
  > 
  Goodbye!

Let rec with unused recursive variable

  $ echo 'let rec x = x in 1' | dune exec miniML
  miniML REPL
  > 1 : Int
  > 
  Goodbye!

Capture-avoiding substitution (rejected by typechecker, z is unbound)

  $ echo '(\x z.x) z 0' | dune exec miniML
  miniML REPL
  > Typecheker error: Unbound variable: "z"
  > 
  Goodbye!
