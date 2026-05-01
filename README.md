# MiniML

Минималистичный функциональный язык программирования, написанный на OCaml.

## Возможности

- **Let-выражения** — связывание имён со значениями, в том числе рекурсивные (`let rec`)
- **Функции** — лямбда-абстракции с несколькими аргументами (`\x y -> ...`)
- **Условия** — if-then-else с логическим ветвлением
- **Операторы**:
  - арифметические: `+`, `-`, `*`, `/`
  - сравнения: `=`, `!=`, `<`, `<=`, `>`, `>=`
  - логические: `&&`, `||`, `!`, `^` (xor)
- **Типы**: целые числа, булевы значения `true`/`false`

## Синтаксис

Полный синтаксис языка описан в [docs/grammar.md](docs/grammar.md).

Примеры:

```
let x = 5 in x + 1
=> 6

let rec fact = \n -> if n = 0 then 1 else n * (fact (n - 1)) in fact 5
=> 120

let add = \x y -> x + y in add 3 4
=> 7
```

## Установка

```bash
opam install . --deps-only
dune build
```

## Использование

### REPL

Запустите интерактивный режим:

```bash
dune exec miniML
```

Команды REPL:
- `:h`, `:help` — показать справку
- `:q`, `:quit` — выйти

### Как библиотека

```ocaml
open MiniML

let result =
  match Lexer.tokenize "1 + 2" with
  | Ok tokens ->
    (match Parser.parse tokens with
     | Ok ast -> Interpreter.eval [] ast
     | Error e -> Error e)
  | Error e -> Error e
```

## Тесты

```bash
dune runtest
```

Покрытие:

```bash
dune runtest --instrument-with bisect_ppx
bisect-ppx-report html #отчет будет в _coverage/
```

## Лицензия

MIT License — см. [LICENSE](LICENSE).
