type compOp =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type 'a comparison = 'a * compOp * 'a

type 'a predicate =
  | Comp of 'a comparison
  | Or of 'a predicate * 'a predicate
  | Not of 'a predicate

type 'a t = Pred of 'a predicate | And of 'a t * 'a t

(* compTrans either returns Some(transitive comparison expression) from two
 * given comparison expressions or None if there are no additional transitive
 * expressions.
 * E.g. compTrans(A = B, B < C) -> A < C *)
val compTrans : 'a comparison -> 'a comparison -> 'a comparison option

(* stringComp returns the string representation of the comparison.  It takes
 * the string function for the variant type and the comparison expression
 * itself. *)
val stringComp : ('a -> string) -> 'a comparison option -> string

(* predTrans returns transitive predicates from two given predicate expressions
 * (non-CNF). The two predicate expressions are assumed to be disjoint in order
 * to generate non-duplicate predicates.
 * E.g. predTrans(A = B, A < 1 OR C = D) -> B < 1 OR C = D *)
val predTrans : 'a predicate -> 'a predicate -> 'a predicate list

(* cnfTrans returns transitive CNFs from two given CNFs. The two CNFs
 * are assumed to be disjoint in order to generate non-duplicate CNFs.
 * E.g. cnfTrans(A = B AND C < D, B < C) -> (A < C AND A < D AND B < C) *)
val cnfTrans : 'a t -> 'a t -> 'a t option
