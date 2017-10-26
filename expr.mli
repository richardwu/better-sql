type compOp =
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | Ne

type joinType =
  | Inner
  | Full
  | Left
  | Right

(* Note: all props are "options" - this allows us to use None to denote
 * that a prop has not been derived yet; otherwise we can use the memoized
 * prop *)
type props =
  Props of
  ColumnsPerTable.t Columns.t option   (* output columns (multiple tables) of an expression *)
  (* * some other prop *)

type predicate =
  | Compare of t * compOp * t
  | Or of predicate * predicate
  | Not of predicate
and cnf =
  | Pred of predicate
  (* right-most derivation prevents ambiguity: this may be useful later *)
  | And of predicate * cnf
(* TODO(richardwu): Is it better to have a list of children 'expr' instead of a
 * fixed cross-product of them? *)
(* With a list, we can avoid pattern matching every expr when updating properties
 * or when performing transformations *)
(* Then again, the power of a functional language is precise pattern matching *)
and op =
  | SELECT of
    predicate       (* filter expression *)
    * t
  | PROJECT of
    ColumnsPerTable.t Columns.t       (* set of columns across multiple tables *)
    * t
  | JOIN of
    joinType
    * predicate     (* join predicate *)
    * t          (* left *)
    * t          (* right *)
  | SCAN of
    Table.t
  | CONST
(* TODO(richardwu): we could simply append 'props' to every 'op' and make
 * this 'expr' *)
and t =
  | Expr of op * props     (* an expr is an op with properties *)

(* fromOp returns an expression with the given op and no properties *)
val fromOp : op -> t

(* columns returns the output columns of an expression *)
val columns : t -> ColumnsPerTable.t Columns.t
