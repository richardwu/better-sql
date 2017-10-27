open Cnf
open Expr

let e1 = fromOp (SCAN(Table.fromName "a"))
let _ = Columns.prettyprint (columns e1)

let e2 = fromOp
  (JOIN(
    Inner,
    Pred(Comp(fromOp CONST, Eq, fromOp CONST)),
    fromOp (SCAN(Table.fromName "a")),
    fromOp (SCAN(Table.fromName "b"))
  ))
let _ = Columns.prettyprint (columns e2)
