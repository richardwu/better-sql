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

(* compTrans returns the transitive result (option) for two given comparison
 * expression.
 * E.g. compTrans(A = B, B < C) -> A < C *)
val compTrans : 'a compare -> 'a compare -> 'a compare option

(* predTrans returns the transitive result for two given predicate expressions (non-CNF)
 * E.g. predTrans(A = B, A < 1 OR C = D) -> B < 1 OR C = D *)
val predTrans : 'a predicate -> 'a predicate -> 'a predicate option

(* cnfTrans returns the transitive CNF result of two CNFs
 * E.g. cnfTrans(A = B AND C < D, B < C) -> (A < C AND A < D AND B < C) *)
val cnfTrans : 'a t -> 'a t -> 'a t option
