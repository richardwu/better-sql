include Map.Make(Table)

let union mapA mapB =
  let union_columns table colsA colsB =
    Some(ColumnsPerTable.union colsA colsB) in
  union union_columns mapA mapB

let all table =
  singleton table (ColumnsPerTable.all table)

let intersect mapA mapB =
  let intersect_columns table colsAOpt colsBOpt =
    match colsAOpt, colsBOpt with
    | None, _
    | _, None -> None
    | Some(colsA), Some(colsB) ->
        Some(ColumnsPerTable.intersect colsA colsB) in
    merge intersect_columns mapA mapB

let prettyprint map =
  let print_h table columns =
    let _ = ColumnsPerTable.prettyprint columns in
    ignore (print_endline "") in
  iter print_h map
