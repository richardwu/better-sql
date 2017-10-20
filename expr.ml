type compOp =
   Eq
  | Lt
  | Gt
  | Le
  | Ge
  | Ne

type table =
  Table of string    (* name *)

type joinType =
  | Inner
  | Full
  | Left
  | Right

type predicate = Compare of compOp * expr * expr
and expr =
  | SELECT of
    expr
  | PROJECT of
    predicate
    * expr
  | JOIN of
    joinType
    * predicate
    * expr    (* left *)
    * expr    (* right *)
  | SCAN of
    table
  (* TODO: use varid or something *)
  | VAR of
    table
  | CONST of
    int
