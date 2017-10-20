let basicQuery (pred:predicate) (joinExpr:expr) =
  SELECT(PROJECT(pred, joinExpr))

let basicComp (table:string) (const:int) =
  Compare(Eq, VAR(Table table), CONST(const))

let basicJoin (tableA:string) (tableB:string) =
  JOIN(Inner, Compare(Eq, VAR(Table tableA), VAR(Table tableB)), SCAN(Table tableA), SCAN(Table tableB))

(* SELECT * FROM a JOIN b WHERE a.X = 1 *)
let q1 = basicQuery (basicComp "a" 1) (basicJoin "a" "b")
(* SELECT * FROM (a : a.X = 1) JOIN (b : b.X = 1) WHERE a.X = 1 *)
let q1pushed = pushdown q1 None

(* SELECT * FROM a JOIN b WHERE b.X = 1 *)
let q2 = basicQuery (basicComp "b" 1) (basicJoin "a" "b")
(* SELECT * FROM (a : a.X = 1) JOIN (b : b.X = 1) WHERE b.X = 1 *)
let q2pushed = pushdown q2 None

(* SELECT * FROM b JOIN c WHERE a.X = 1 ; this not valid but simply to test pushdown *)
let q3 = basicQuery (basicComp "a" 1) (basicJoin "b" "c")
(* SELECT * FROM b JOIN c WHERE a.X = 1 *)
let q3pushed = pushdown q3 None

