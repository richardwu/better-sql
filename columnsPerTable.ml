(* ColumnsPerTable only shows the columns for ONE table *)
(* this is an internal module; please refer to Columns (columns.mli) *)
type t = Columns of Table.t * int

(* bitmap where all bits are 1 *)
let allIdxs =  -1

let all table =
  Columns(table, allIdxs)

let table columns =
  let Columns(table, _) = columns in
  table

let union colsA colsB =
  let Columns(tableA, idxsA), Columns(tableB, idxsB) = colsA, colsB in
  assert (tableA = tableB);
  Columns(tableA, idxsA lor idxsB) (* bitwise OR *)

let intersect colsA colsB =
  let Columns(tableA, idxsA), Columns(tableB, idxsB) = colsA, colsB in
  assert (tableA = tableB);
  Columns(tableA, idxsA land idxsB) (* bitwise AND *)

let prettyprint cols =
  let Columns(table, idxs) = cols in
  let _ = Table.prettyprint table in
  let _ = print_string " - cols: " in
  ignore (print_int idxs)

