open OUnit2
open Cnf

type test =
  | A
  | B
  | C
  | D

let stringOf = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"

type compTestArgs =
  { l : test comparison
  ; r : test comparison
  ; expected: test comparison option
  }

let compTCs =
  [
    (* Left equality, right any op *)
    {l=(A, Eq, B); r=(B, Eq, C); expected=Some(A, Eq, C)}; (* A = B, B = C --> A = C *)
    {l=(A, Eq, B); r=(B, Lt, C); expected=Some(A, Lt, C)}; (* A = B, B < C --> A < C *)
    {l=(A, Eq, B); r=(B, Le, C); expected=Some(A, Le, C)}; (* A = B, B <= C --> A <= C *)
    {l=(A, Eq, B); r=(B, Gt, C); expected=Some(A, Gt, C)}; (* A = B, B > C --> A > C *)
    {l=(A, Eq, B); r=(B, Ge, C); expected=Some(A, Ge, C)}; (* A = B, B >= C --> A >= C *)
    (* No inherent transitive relation *)
    {l=(A, Eq, B); r=(C, Eq, D); expected=None}; (* A = B, C = D --> None *)
    {l=(A, Eq, B); r=(C, Lt, D); expected=None}; (* A = B, C < D --> None *)
    {l=(A, Eq, B); r=(C, Le, D); expected=None}; (* A = B, C <= D --> None *)
    {l=(A, Eq, B); r=(C, Gt, D); expected=None}; (* A = B, C > D --> None *)
    {l=(A, Eq, B); r=(C, Ge, D); expected=None}; (* A = B, C >= D --> None *)
    (* Duplicates should return None *)
    {l=(A, Eq, B); r=(B, Eq, A); expected=None}; (* A = B, B = A --> None *)
    {l=(B, Eq, B); r=(B, Eq, A); expected=None}; (* B = B, B = A --> None *)
    {l=(A, Le, B); r=(B, Ge, A); expected=None}; (* A <= B, B >= A --> None *)
    (* Non-equality, same left/right ops *)
    {l=(A, Ne, B); r=(B, Ne, C); expected=None}; (* A != B, B != C --> None *)
    {l=(A, Lt, B); r=(B, Lt, C); expected=Some(A, Lt, C)}; (* A < B, B < C --> A < C *)
    {l=(A, Le, B); r=(B, Le, C); expected=Some(A, Le, C)}; (* A <= B, B <= C --> A <= C *)
    {l=(A, Gt, B); r=(B, Gt, C); expected=Some(A, Gt, C)}; (* A > B, B > C --> A > C *)
    {l=(A, Ge, B); r=(B, Ge, C); expected=Some(A, Ge, C)}; (* A >= B, B >= C -->= C >= A *)
    {l=(A, Lt, B); r=(C, Gt, B); expected=Some(A, Lt, C)}; (* A < B, C > B --> A < C *)
    {l=(A, Lt, B); r=(B, Le, C); expected=Some(A, Lt, C)}; (* A < B; B <= C --> A < C *)
    {l=(A, Gt, B); r=(B, Ge, C); expected=Some(A, Gt, C)}; (* A > B; B >= C --> A > C *)
    {l=(A, Lt, B); r=(C, Lt, D); expected=None}; (* A < B, C < D --> None *)
  ]
let compTests testCtx =
  let testComp i args =
    assert_equal ~msg:("test case " ^ (string_of_int (i+1))) ~printer:(stringComp stringOf) args.expected (compTrans args.l args.r) in
  List.iteri testComp compTCs

let () = assert ((predTrans (Comp(A, Eq, B)) (Not(Comp(B, Lt, C)))) = [Not(Comp(A, Lt, C))])

let suite =
  "cnfSuite">:::
    ["compTests">:: compTests]

let () =
  run_test_tt_main suite
