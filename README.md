# BetterSQL™ - an attempt at a SQL optimizer

This is a (very) rough prototype for SQL optimization in OCaml.

## Build

You will first need to install [OCaml](https://ocaml.org/docs/install.html)
and [OPAM](https://opam.ocaml.org/doc/Install.html).

On macOS with [Homebrew](https://brew.sh/)
```
brew intall ocaml
brew install opam
```

Clone this project
```sh
git clone https://github.com/richardwu/better-sql.git
```

and run
```sh
make setup
make build
```

## Interactive

To interactively test this, simply download
[utop](https://github.com/diml/utop), and load the necessary definitions from
the source files. For example (in topological order of dependencies):
```ocaml
#use "table.ml";;

#use "columnsPerTable.ml";;

#use "columns.ml";;

#use "expr.ml";;

open Expr;;    (* elides the need to prefix with module name *)
let e1 = fromOp(SCAN(Table.fromName "a"));;
Columns.prettyprint (columns e1);;
```

## Testing

```
make test
```
