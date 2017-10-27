type joinType =
  | Inner
  | Full
  | Left
  | Right

type props =
  Props of
  ColumnsPerTable.t Columns.t option

type op =
  | SELECT of
    t Cnf.t
    * t
  | PROJECT of
    ColumnsPerTable.t Columns.t
    * t
  | JOIN of
    joinType
    * t Cnf.t
    * t
    * t
  | SCAN of
    Table.t
  | CONST
and t =
  | Expr of op * props

let fromOp op =
  Expr(op, Props(None))

let rec columns expr =
  let Expr(node, (Props(columnProp) as props)) = expr in
  match columnProp with
  (* Columns already memoized, return *)
  | Some(cols) -> cols
  (* Column prop hasn't been updated, recursively update it *)
  | None ->
      begin match node with
      (* Filter columns from expr' by taking the intersect with the projection *)
      | PROJECT(cols, expr') -> Columns.intersect cols (columns expr')
      (* Selection doesn't restrict columns *)
      | SELECT(_, expr') -> columns expr'
      (* union the columns from left and right *)
      | JOIN(_, _, l, r) -> Columns.union (columns l) (columns r)
      | SCAN(table) -> Columns.all table
      | CONST -> Columns.empty
      end
