type t = Table of string

(* fromName returns a Table.t with the table name *)
val fromName : string -> t

(* compare is more of a necessity to use Table.t as a key in a Map *)
val compare : t -> t -> int

(* prettyprint prints to STDOUT the table WITHOUT an ending newline *)
val prettyprint : t -> unit
