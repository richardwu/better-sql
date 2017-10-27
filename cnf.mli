type compOp =
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | Ne

type 'a compare = 'a * compOp * 'a

type 'a predicate =
  | Comp of 'a compare
  | Or of 'a predicate * 'a predicate
  | Not of 'a predicate

(* Right recursive allows us iterate through it like a "list" *)
type 'a t = Pred of 'a predicate | And of 'a predicate * 'a t

val compTrans : 'a compare -> 'a compare -> 'a compare option

val predTrans : 'a predicate -> 'a predicate -> 'a predicate option

val cnfTrans : 'a t -> 'a t -> 'a t option
