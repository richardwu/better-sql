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

let rec compTrans compA compB =
  match compA, compB with
  (* Eq is a special case since we may create transitive expressions with any
   * other comparison expression *)
  | (eqL, Eq, eqR), (otherL, otherOp, otherR)
  | (otherL, otherOp, otherR), (eqL, Eq, eqR) ->
      if eqL = eqR then
        (* the equality expression is trivial, no transitivity *)
        None
      (* The following 4 conditions simply replaces the equivalent expression
       * otherX with one of eqX *)
      else if eqL = otherL then
        Some(eqR, otherOp, otherR)
      else if eqL = otherR then
        Some(otherL, otherOp, eqR)
      else if eqR = otherL then
        Some(eqL, otherOp, otherR)
      else if eqR = otherR then
        Some(otherL, otherOp, eqL)
      else
        None
  | (l, op, r), (l', op', r') ->
    if op = op' then
      begin match op with
      (* Eq is not possible since it's checked for above, but we may as
       * well include it here with Ne *)
      | Eq
      | Ne ->
        (* A = B, A = C --> B = C *)
        if l = l' && r != r' then
          Some(r, op, r')
        (* A = B, C = A --> B = C *)
        else if l = r' && r != l' then
          Some(r, op, l')
        (* A = B, B = C --> A = C *)
        else if r = l' && l != r' then
          Some(l, op, r')
        (* A = B, C = B --> A = C *)
        else if r = r' && l != l' then
          Some(l, op, l')
        else
          None
      | Lt
      | Le ->
        (* A < B, B < C --> A < C *)
        if r = l' && l != r' then
          Some(l, op, r')
        else
          None
      | Gt ->
        compTrans (r, Lt, l) (r', Lt, l')
      | Ge ->
        compTrans (r, Le, l) (r', Le, l')
      end
    else
      (* We can sometimes make the ops equivalent by reversing their operands *)
      (* Ideally, we want both of them to be Lt or Le *)
      (* A > B --> B < A *)
      (* A >= B --> B <= A *)
      begin match op, op' with
      | (Lt as replace), Gt
      | (Le as replace), Ge ->
          compTrans compA (r', replace, l')
      | Gt, (Lt as replace)
      | Ge, (Le as replace) ->
          compTrans (r, replace, l) compB
      (* TODO(richardwu): A Lt B, B Le C --> A < C is also transitive
       * (similarly for Gt/Ge *).
      (* Note: Eq/Other expressions were handled in the first conditional *)
      | _ -> None
      end

let rec predTrans predA predB =
  match predA, predB with
  | Comp(compA), Comp(compB) ->
      begin match compTrans compA compB with
      | None -> None
      | Some(result) -> Some(Comp(result))
      end
  | Not(pred'), pred
  | pred, Not(pred') ->
      begin match predTrans pred pred' with
      | None -> None
      | Some(result) -> Some(Not(result))
      end
  | Or(pred1, pred2), pred
  | pred, Or(pred1, pred2) ->
      begin match (predTrans pred pred1), (predTrans pred pred2) with
      | None, None -> None
      (* Only one part of the OR produced a valid
       * transitive match, we need to return the original
       * side and the transitive side *)
      | None, Some(result2) ->
          Some(Or(pred1, result2))
      | Some(result1), None ->
          Some(Or(result1, pred2))
      (* Both sides produced transitive expressions.
       * TODO(richardwu): do we need to do Or(pred1, result2) and Or(result1,
       * pred2)
       * I think we do: to generate the power sets of these predicates with
       * their transitive results.
       * That is: Or(pred1, result2), Or (result1, pred2) and Or(result1,
       * result2) are all distinct transitive results.
       * Maybe return an 'a predicate option list? *)
      | Some(result1), Some(result2) ->
          Some(Or(result1, result2))
      end

let rec cnfTrans cnfA cnfB =
  match cnfA, cnfB with
  | Pred(predA), Pred(predB) ->
      begin match predTrans predA predB with
      | None -> None
      | Some(pred) -> Some(Pred(pred))
      end
  | And(first, rest), (Pred(pred) as other)
  | (Pred(pred) as other), And(first, rest) ->
      begin match predTrans first pred with
      (* continue with rest of CNF *)
      | None -> cnfTrans other rest
      | Some(result) ->
          begin match cnfTrans other rest with
          | None -> Some(Pred(result))
          | Some(restResult) -> Some(And(result, restResult))
          end
      end
  | And(pred, cnf), And(pred', cnf') ->
      begin match predTrans pred pred' with
      | None -> cnfTrans cnf cnf'
      | Some(result) ->
          begin match cnfTrans cnf cnf' with
          | None -> Some(Pred(result))
          | Some(restResult) -> Some(And(result, restResult))
          end
      end
