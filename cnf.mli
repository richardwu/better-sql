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
  | Not of 'a predicate
  | Or of 'a predicate * 'a predicate

type 'a t = Pred of 'a predicate | And of 'a t * 'a t

(* stringComp returns the string representation of the comparison.
 * It takes the string function for the variant type and the comparison
 * expression itself. *)
val stringComp : ('a -> string) -> 'a comparison -> string
(* stringCompOpt is similar to stringComp but unwraps the option constructor. *)
val stringCompOpt : ('a -> string) -> 'a comparison option -> string

(* stringPred returns the string representation of the predicate. See
 * stringComp for parameters. *)
val stringPred: ('a -> string) -> 'a predicate -> string
(* stringPredList is similar to stringPred but from a predicate list. *)
val stringPredList: ('a -> string) -> 'a predicate list -> string

(* stringCNF returns the string representation of the CNF. See stringComp for
 * parameters. *)
val stringCNF: ('a -> string) -> 'a t -> string
(* stringCNFOpt is similar to stringCNF but unwraps the option constructor. *)
val stringCNFOpt: ('a -> string) -> 'a t option -> string

(* predListEq provides an equality comparison between two predicate lists
 * where order does not matter.
 * It returns true iff there is a 1-1 correspondence between every predicate in
 * both lists.
 * It always checks for structural equality first. *)
val predListEq : 'a predicate list -> 'a predicate list -> bool

(* cnfEq provides an equality comparison between two CNFs where order of the
 * predicates with AND clauses does not matter.
 * It returns true iff there is a 1-1 correspondence between every Pred component
 * of both CNFs.
 * It always checks for structural equality first. *)
val cnfEq : 'a t -> 'a t -> bool

(* cnfOptEq is the cnfEq equivalent but for CNF options. *)
val cnfOptEq : 'a t option -> 'a t option -> bool

(* fromPredList is a helper function that returns the CNF expression for a list
 * of simple predicates ANDed together
 * E.g. fromPredList [A = B, C < D] --> A = B AND C < D *)
val fromPredList : 'a predicate list -> 'a t option

(* compTrans either returns Some(transitive comparison expression) from two
 * given comparison expressions or None if there are no additional transitive
 * expressions.
 * E.g. compTrans(A = B, B < C) -> A < C *)
val compTrans : 'a comparison -> 'a comparison -> 'a comparison option

(* predTrans returns transitive predicates from two given predicate expressions
 * (non-CNF). The two predicate expressions are assumed to be disjoint in order
 * to generate non-duplicate predicates.
 * E.g. predTrans(A = B, A < 1 OR C = D) -> B < 1 OR C = D *)
val predTrans : 'a predicate -> 'a predicate -> 'a predicate list

(* cnfTrans returns transitive CNFs from two given CNFs. The two CNFs
 * are assumed to be disjoint in order to generate non-duplicate CNFs.
 * E.g. cnfTrans(A = B AND C < D, B < C) -> (A < C AND A < D AND B < C) *)
val cnfTrans : 'a t -> 'a t -> 'a t option
