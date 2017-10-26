open Expr

let rec applyPreds expr preds =
  match preds with
  | [] -> expr
  | first :: rest ->
    match first with
    | Compare(_, VAR(table), (CONST(_) as const))
    | Compare(_, (CONST(_) as const), VAR(table)) ->
      match expr with
      | SCAN(childTable) ->
        if childTable = table then
          (* apply selection right after SCAN *)
          SELECT(first, expr)
        else
          (* try next predicate *)
          applyPreds expr rest
      | JOIN(Inner, (Compare(Eq, VAR(tableL), VAR(tableR)) as joinPred), left, right) ->
          (* since table (from predicate) is in JOIN eq, create a new predicate and recurse *)
          if table = tableL || table = tableR then
            let newPreds = (Compare(Eq, VAR(tableL), const)) :: (Compare(Eq, VAR(tableR), const)) :: rest in
            let left' = applyPreds left newPreds in
            let right' = applyPreds right newPreds in
            JOIN(Inner, joinPred, left', right')
          else
            (* try next predicate *)
            applyPreds expr rest
      (* Not SCAN nor JOIN *)
      (* TODO: handle other intermediate exprs *)
      | _ -> expr
    (* non-equality predicate, try rest *)
    (* TODO: handle non-equality predicates *)
    | _ -> applyPreds expr rest

let rec pushdown (expr:expr) (pred:predicate option) : expr =
  match expr with
  | PROJECT(expr') ->
      PROJECT(pushdown expr' pred)
  | SELECT(projPred, expr') ->
      (* New predicate to push down *)
      (* TODO: check if selection is applied, unwrap this *)
      (* TODO: accumulate these selections and push them down *)
      SELECT(projPred, pushdown expr' (Some projPred))
  (* Once a JOIN is encountered, we just apply the predicate based on the equality *)
  | JOIN(_, _, _, _) -> match pred with
    | None -> expr (* no predicates to push down *)
    | Some(pred) -> applyPreds expr [pred]
