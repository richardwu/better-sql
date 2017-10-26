(* The type of Columns is effectively a map from tables to their columns *)
type 'a t = 'a Map.Make(Table).t

(* empty returns an empty set of columns *)
val empty : ColumnsPerTable.t t

(* all returns a set of all columns for a given table *)
val all : Table.t -> ColumnsPerTable.t t

(* union returns the logical union of two sets of columns *)
val union : ColumnsPerTable.t t -> ColumnsPerTable.t t -> ColumnsPerTable.t t

(* intersect returns the logical intersection of two sets of columns *)
val intersect : ColumnsPerTable.t t -> ColumnsPerTable.t t -> ColumnsPerTable.t t

(* prettyprint prints to STDOUT the set of columns WITH an ending newline *)
val prettyprint : ColumnsPerTable.t t -> unit
