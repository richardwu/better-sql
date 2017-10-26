type t = Columns of
  Table.t
  * int (* bitmap for column indices *)

(* all returns the set of all columns for a given table *)
val all : Table.t -> t

(* table returns the table from which the columns are derived *)
val table : t -> Table.t

(* union returns the logical union of two sets of columns from the SAME table *)
val union : t -> t -> t

(* intersect returns the logical intersection of two sets of columns from the SAME table *)
val intersect : t -> t -> t

(* prettyprint prints to STDOUT the columns WITHOUT an ending newline *)
val prettyprint : t -> unit
