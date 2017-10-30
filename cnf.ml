(* Types *)
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

(* String functions *)

let stringOfOp = function
  | Eq -> "="
  | Ne -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="

let stringComp strConv (l, op, r) =
  (strConv l) ^ " " ^ stringOfOp op ^ " " ^ (strConv r)

let stringCompOpt strConv = function
  | None -> "None"
  | Some(comp) -> stringComp strConv comp

let rec stringPred strConv = function
  | Comp(comp) ->
      stringComp strConv comp
  | Not(pred) ->
      "¬(" ^ stringPred strConv pred ^ ")"
  | Or(pred1, pred2) ->
      "(" ^ stringPred strConv pred1 ^ ") ∨ (" ^ stringPred strConv pred2 ^ ")"

let stringPredList strConv = function
  | [] -> "[]"
  | predList ->
      "[" ^ String.concat ", " (List.map (stringPred strConv) predList) ^ "]"

let rec stringCNF strConv = function
  | Pred(pred) ->
      stringPred strConv pred
  | And(cnf1, cnf2) ->
      "(" ^ stringCNF strConv cnf1 ^ ") ∧ (" ^ stringCNF strConv cnf2 ^ ")"

let stringCNFOpt strConv = function
  | None -> "None"
  | Some(cnf) ->
      stringCNF strConv cnf

(* Helper functions *)

let negateOp = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt

let rec unwrapNot = function
  | Not(Not(inner)) -> unwrapNot inner
  | Not(Comp(l, op, r)) -> Comp(l, (negateOp op), r)
  (* TODO(richardwu): We can use DeMorgan's Law to convert ¬(A OR B) --> ¬A AND
   * ¬B; however, we can't currently do this since AND's are part of the CNF
   * type.
   * The other option is to return a list of predicates (semantically the
   * same). *)
  | pred -> pred

(* Equality functions *)

let predListEq predsA predsB =
  if predsA = predsB then
    true
  else if (List.compare_lengths predsA predsB) != 0 then
    false
  else
    (* O(n^2) check to see if there exists an element of predsA (predA) in
    * predsB for all predA) *)
    List.for_all (fun predA -> List.mem predA predsB) predsA

let cnfEq cnfA cnfB =
  (* containedIn checks if cnfA is a subset of cnfB, where each element of
   * a CNF is defined as a Pred(). *)
  let rec containedIn cnfA cnfB =
    begin match cnfA, cnfB with
    | And(cnf1, cnf2), And(_, _) ->
        (* We want to break down the LHS until eventually we get Pred()s,
         * which we can then apply the next match case *)
        (containedIn cnf1 cnfB) && (containedIn cnf2 cnfB)
    | Pred(_), And(cnf1, cnf2) ->
        (* We break down the RHS now to until we get only Pred()s to compare
         * with the LHS, which is the next match case *)
        (containedIn cnfA cnf1) || (containedIn cnfA cnf2)
    | Pred(predA), Pred(predB) ->
        predA = predB
    | And(_, _), Pred(_) ->
        (* 2+ predicates cannot possibly be contained in 1 predicate *)
        false
    end in
  if cnfA = cnfB then
    true
  else
    (* TODO(richardwu): this may be more efficient (although requires more
     * code) to just check one "containsIn" then check for equal # of Preds.
     * This passes for duplicates but it shouldn't. *)
    (containedIn cnfA cnfB) && (containedIn cnfB cnfA)

let cnfOptEq cnfAOpt cnfBOpt =
  match cnfAOpt, cnfBOpt with
  | None, None ->
      true
  | None, _
  | _, None ->
      false
  | Some(cnf1), Some(cnf2) ->
      cnfEq cnf1 cnf2

(* Transitive functions *)

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
      else if eqL = otherL && eqR != otherR then
        Some(eqR, otherOp, otherR)
      else if eqL = otherR && otherL != eqR then
        Some(otherL, otherOp, eqR)
      else if eqR = otherL && eqL != otherR then
        Some(eqL, otherOp, otherR)
      else if eqR = otherR && otherL != eqL then
        Some(otherL, otherOp, eqL)
      else
        None
  | (l, op, r), (l', op', r') ->
    if op = op' then
      begin match op with
      (* Eq case was handled above *)
      | Eq
      | Ne ->
          (* Not equal is not always transitive, and is only transitive on
           * sets with at most one element *)
          None
      | Lt
      | Le
      | Gt
      | Ge ->
        (* A < B, B < C --> A < C *)
        (* A <= B, B <= C --> A <= C *)
        (* A > B, B > C --> A > C *)
        (* A >= B, B >= C --> A >= C *)
        if r = l' && l != r' then
          Some(l, op, r')
        (* A < B, C < A --> C < A *)
        (* A <= B, C <= A --> C <= A *)
        (* A > B, C > A --> C > B *)
        (* A >= B, C >= A --> C >= B *)
        else if l = r' && l' != r then
          Some(l', op, r)
        else
          None
      end
    else
      begin match op, op' with
      | (Lt as replace), Gt
      | (Le as replace), Ge
      | (Gt as replace), Lt
      | (Ge as replace), Le ->
          (* We can sometimes make the ops equivalent by reversing their operands
           * so we can nicely recurse. *)
          (* Ideally, we want both of them to be that of the first comp. *)
          compTrans compA (r', replace, l')
      | (Lt as replace), Le
      | Le, (Lt as replace)
      | (Gt as replace), Ge
      | Ge, (Gt as replace) ->
          (* We always replace with the strict inequality since it's more restrictive *)
          (* A < B, B <= C --> A < C *)
          (* A <= B, B < C --> A < C *)
          (* A > B, B >= C --> A > C *)
          (* A >= B, B > C --> A > C *)
          if r = l' && l != r' then
            Some(l, replace, r')
          (* A < B, C <= A --> C < B *)
          (* A <= B, C < A --> C < B *)
          (* A > B, C >= A --> C > B *)
          (* A >= B, C > A --> C > B *)
          else if l = r' && l' != r then
            Some(l', replace, r)
          else
            None
      | Lt, Ge ->
          (* Flip the op of compB and recurse *)
          compTrans compA (r', Le, l')
      | Ge, Lt ->
          (* Flip the op of compB and recurse *)
          compTrans compA (r', Gt, l')
      (* Note: Eq/Other expressions were handled in the first conditional *)
      | _, _ -> None
      end

let rec predTrans predA predB =
  (* We prefer Comp/Or expressions without NOTs since it's
   * easier to reason about them (i.e. our compTrans is more complete). *)
  let predA = unwrapNot predA in
  let predB = unwrapNot predB in
  match predA, predB with
  | Comp(compA), Comp(compB) ->
      begin match compTrans compA compB with
      | None -> []
      | Some(result) -> [Comp(result)]
      end
  | Not(_), _
  | _, Not(_) ->
      (* This should only happen when OR is inside the NOT since other
       * predicates are unwrapped (see comment above func unwrapNot).
       * We can't really propagate transitivity inside NOTs.
       * counterexample: A < B, ¬(B < C) does NOT imply ¬(A < C)
       *  for A = C = 1, B = 2.
       * Instead, we unwrap these NOTs beforehand. *)
      []
  | Or(_, _), Or(_, _) ->
      (* We can't distribute ORs because either sub-predicate could be
       * false. *)
      []
  | Or(pred1, pred2), pred
  | pred, Or(pred1, pred2) ->
      begin match (predTrans pred pred1), (predTrans pred pred2) with
      | [], [] -> []
      | [], results2 ->
          (* Only one part of the OR produced a transitive match, we return the
           * original side and the transitive side. *)
          List.map (fun res2 -> Or(pred1, res2)) results2
      | results1, [] ->
          (* similar to the previous match *)
          List.map (fun res1 -> Or(res1, pred2)) results1
      | results1, results2 ->
          (* Both sides produced transitive expressions, take cross product of
           * transitive pairings. *)
          let iterResults1 res1 =
            List.flatten(List.map (fun res2 -> [Or(pred1, res2); Or(res1, pred2); Or(res1, res2)]) results2) in
          List.flatten(List.map iterResults1 results1)
      end

let rec fromPredList predList =
  match predList with
  | [] -> None
  | h :: t ->
      begin match fromPredList t with
      | None -> Some(Pred(h))
      | Some(result) -> Some(And(Pred(h), result))
      end

(* fromCNFList is a helper function that generates the CNF expression
 * from a list of CNF expressions
 * E.g. fromCNFList [A = B AND C = D, A < 1] --> A = B AND C = D AND A < 1 *)
let rec fromCNFList cnfList =
  match cnfList with
  | [] -> None
  | h :: t ->
      begin match fromCNFList t with
      | None -> Some(h)
      | Some(result) -> Some(And(h, result))
      end

let rec cnfTrans cnfA cnfB =
  match cnfA, cnfB with
  | Pred(predA), Pred(predB) ->
      (* Both are basic Pred (no ANDs) *)
      fromPredList (predTrans predA predB)
  | And(half1, half2), (Pred(_) as pred)
  | (Pred(_) as pred), And(half1, half2) ->
      (* One is a basic Pred and the other is a CNF with 1+ ANDs
       * We distributively apply the Pred across both CNFs in the AND clause. *)
      begin match (cnfTrans pred half1), (cnfTrans pred half2) with
      | None, None ->
          (* Neither side could generate a transitive CNF, we return None *)
          None
      | (Some(_) as result), None
      | None, (Some(_) as result) ->
          (* Only one side returned a new transitive CNF, we return just that *)
          result
      | Some(result1), Some(result2) ->
          (* Both sides returned a new transitive CNF, so we must return them
           * ANDed *)
          Some(And(result1, result2))
      end
  | And(half1, half2), And(half3, half4) ->
      (* Two AND CNFs, use F-O-I-L *)
      let results =
        begin match cnfTrans half2 half4 with
        | None -> []
        | Some(result4) -> [result4]
        end in
      let results =
        begin match cnfTrans half2 half3 with
        | None -> results
        | Some(result3) -> result3 :: results
        end in
      let results =
        begin match cnfTrans half1 half4 with
        | None -> results
        | Some(result2) -> result2 :: results
        end in
      let results =
        begin match cnfTrans half1 half3 with
        | None -> results
        | Some(result1) -> result1 :: results
        end in
      fromCNFList results
