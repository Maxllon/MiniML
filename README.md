# MiniML

Минималистичный функциональный язык программирования, написанный на OCaml.

## Возможности

- **Let-выражения** — связывание имён со значениями, в том числе рекурсивные (`let rec`), с синтаксическим сахаром для нескольких аргументов (`let f x y = ... in ...`); let-полиморфизм (значения обобщаются по схеме Hindley–Milner)
- **Функции** — лямбда-абстракции без явной аннотации типов (`\x y. ...`)
- **Условия** — `if`-`then`-`else` (строгий CBV) и ленивый вариант `lif`
- **Обработка ошибок** — `try`-`with`
- **Суммарные типы** — декларации `type` с конструкторами и сопоставление с образцом `case` (с проверкой полноты)
- **Кортежи** — `(a, b, c)` и тип `nth` для доступа к элементам
- **Операторы**:
  - арифметические: `+`, `-`, `*`, `/`
  - сравнения: `=`, `!=`, `<`, `<=`, `>`, `>=`
  - логические: `!`/`not`, `&&`/`and`, `||`/`or`, `^`/`xor`
- **Типы**: целые числа, булевы значения `true`/`false`, `unit`, функции, кортежи, пользовательские суммарные типы

## Синтаксис

Полный синтаксис языка описан в [docs/GRAMMAR.md](docs/GRAMMAR.md).

Примеры (вывод соответствует REPL):

```
> let x = 5 in x + 1
6 : Int

> let add = \x y. x + y in add 3 4
7 : Int

> let rec fact n = lif n = 0 then 1 else n * fact (n - 1) in fact 5
120 : Int

> if 5 > 3 then 10 else 20
10 : Int
```

Лямбда-абстракция записывается через точку: `\x y. x + y` — это `\x. \y. x + y`.

Факториал через `lif` не требует оборачивания веток в лямбды — `lif` делает обе ветки ленивыми при строгом CBV, в отличие от обычного `if`.

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

REPL читает выражения построчно, типизирует (упрощённая проверка), компилирует в лямбда-исчисление и выводит результат с типом:

```
> 2 + 3 * 4
14 : Int
```

### Как библиотека

```ocaml
open MiniML

let run input =
  match Lexer.tokenize input with
  | Error e -> Error ("Lexer: " ^ string_of_int e.pos)
  | Ok tokens ->
    match Parser.parse tokens with
    | Error e -> Error e
    | Ok ast ->
      match Typechecker.get_type ast with
      | Error e -> Error e
      | Ok _ ->
        let term = Lambda.ast_to_term ast in
        Ok (Interpreter.eval term)
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
