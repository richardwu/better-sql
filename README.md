# BetterSQL™ - an attempt at a SQL optimizer

This is a (very) rough prototype for SQL optimization in OCaml.

To test this, simply download [utop](https://github.com/diml/utop), and load
the necessary definitions from the source files
```ocaml
#use "expr.ml";;

#use "pushdown.ml";;

#use "main.ml";;
```
