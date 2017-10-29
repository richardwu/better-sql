open OUnit2
open Cnf

type test =
  | A
  | B
  | C
  | D
  | E

let stringOf = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"

(* string functions for l, r arguments *)
let stringCompArg = stringComp stringOf
let stringPredArg = stringPred stringOf
let stringCNFArg = stringCNF stringOf
(* string functions for expected result *)
let stringCompRes = stringCompOpt stringOf
let stringPredRes = stringPredList stringOf
let stringCNFRes = stringCNFOpt stringOf

(* printHeader prints out the test information to STDOUT *)
(* argPrinter requires a toString function for 'l' and 'r' *)
(* resPrinter requires a toString function for 'expected' and 'actual' *)
let printHeader ~argPrinter:ap ~resPrinter:rp ?(cmp = ( = )) testName testNum l r expected actual =
  let () = print_endline ("=== RUN\t" ^ testName ^ " (" ^ string_of_int testNum ^ ")") in
  let () = print_endline ("\tleft: " ^ ap l ^ " - right: " ^ ap r ^ " - expected: " ^ rp expected) in
  let msg =
    if cmp expected actual then
      "\t"
    else
      "(WRONG)\t" in
  print_endline (msg ^ "actual: " ^ rp actual)


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
    {l=(A, Le, B); r=(A, Ge, C); expected=Some(C, Le, B)}; (* A <= B, A >= C --> C <= B *)
    {l=(A, Lt, B); r=(B, Le, C); expected=Some(A, Lt, C)}; (* A < B; B <= C --> A < C *)
    {l=(A, Lt, B); r=(C, Le, A); expected=Some(C, Lt, B)}; (* A < B; C <= A --> C < B *)
    {l=(A, Gt, B); r=(B, Ge, C); expected=Some(A, Gt, C)}; (* A > B; B >= C --> A > C *)
    {l=(A, Lt, B); r=(C, Lt, D); expected=None}; (* A < B, C < D --> None *)
  ]

let compTest testCtx =
  let testComp i args =
    let actual = compTrans args.l args.r in
    let () = printHeader ~argPrinter:stringCompArg ~resPrinter:stringCompRes "compTest" (i+1) args.l args.r args.expected actual in
    assert_equal ~printer:stringCompRes args.expected actual in
  List.iteri testComp compTCs

type predTestArgs =
  { l : test predicate
  ; r : test predicate
  ; expected: test predicate list
  }

(* Converts a comparison test to a predicate test by wrapping parameters in Comp() *)
let compArgsToPredArgs (compArgs:compTestArgs) =
  let predExpected =
    begin match compArgs.expected with
    | None -> []
    | Some(comp) -> [Comp(comp)]
    end in
  {l=Comp(compArgs.l); r=Comp(compArgs.r); expected=predExpected}

let predTestWithComps testCtx =
  let testPred i args =
    let actual = predTrans args.l args.r in
    let () = printHeader ~argPrinter:stringPredArg ~resPrinter:stringPredRes "predTestWithComps" (i+1) args.l args.r args.expected actual in
    assert_equal ~printer:stringPredRes args.expected actual in
  List.iteri testPred (List.map compArgsToPredArgs compTCs)

let predTCs =
  [
    (* NOTs *)
    {l=Comp(A, Eq, B); r=Not(Comp(B, Eq, C)); expected=[Comp(A, Ne, C)]}; (* A = B, ¬(B = C) --> A != C *)
    {l=Not(Comp(A, Ge, B)); r=Comp(B, Eq, C); expected=[Comp(A, Lt, C)]}; (* ¬(A >= B), B = C --> A < C *)
    {l=Comp(A, Lt, B); r=Not(Comp(B, Lt, C)); expected=[]}; (* A < B, ¬(B < C) --> [] *)
    {l=Not(Comp(A, Le, B)); r=Comp(A, Ge, C); expected=[]}; (* ¬(A <= B), A >= C --> [] *)
    {l=Not(Comp(A, Eq, B)); r=Not(Comp(B, Eq, C)); expected=[]}; (* ¬(A = B), ¬(B = C) --> [] *)
    (* ORs *)
    {l=Comp(A, Eq, B); r=Or(Comp(B, Eq, C), Comp(A, Eq, D)); expected=
      [
        Or(Comp(A, Eq, C), Comp(A, Eq, D));
        Or(Comp(B, Eq, C), Comp(B, Eq, D));
        Or(Comp(A, Eq, C), Comp(B, Eq, D));
      ]
    }; (* A = B, B = C ∨ A = D --> [A = C ∨ B = D] *)
    {l=Or(Comp(A, Eq, B), Comp(C, Eq, D)); r=Or(Comp(B, Eq, C), Comp(A, Eq, D)); expected=[]}; (* A = B ∨ C = D, B = C ∨ A = D --> [] *)
  ]

let predTest testCtx =
  let testPred i args =
    let actual = predTrans args.l args.r in
    let () = printHeader ~argPrinter:stringPredArg ~resPrinter:stringPredRes ~cmp:predListEq "predTest" (i+1) args.l args.r args.expected actual in
    assert_equal ~printer:stringPredRes ~cmp:predListEq args.expected actual in
  List.iteri testPred predTCs

type cnfTestArgs =
  { l : test Cnf.t
  ; r : test Cnf.t
  ; expected: test Cnf.t option
  }

(* Converts a predicate test to a CNF test by wrapping parameters in Pred() *)
let predArgsToCNFArgs (predArgs:predTestArgs) =
  let cnfExpected = fromPredList predArgs.expected in
  {l=Pred(predArgs.l); r=Pred(predArgs.r); expected=cnfExpected}

let cnfTestWithPreds testCtx =
  let testCNF i args =
    let actual = cnfTrans args.l args.r in
    let () = printHeader ~argPrinter:stringCNFArg ~resPrinter:stringCNFRes ~cmp:cnfOptEq "cnfTestWithPreds" (i+1) args.l args.r args.expected actual in
    assert_equal ~printer:stringCNFRes ~cmp:cnfOptEq args.expected actual in
  List.iteri testCNF (List.map predArgsToCNFArgs predTCs)

(* shorthand for converting a comparsion e.g. (A, Eq, B) to its CNF form *)
let ctc comp =
  Pred(Comp(comp))

let cnfTCs =
  [
    {l=ctc (A, Eq, B); r=And(ctc (B, Eq, C), ctc (D, Eq, E)); expected=Some(ctc (A, Eq, C))};
    {l=ctc (A, Ne, B); r=And(ctc (B, Eq, C), ctc (D, Eq, E)); expected=Some(ctc (A, Ne, C))};
    {l=ctc (A, Le, B); r=And(ctc (B, Eq, C), ctc (A, Le, C)); expected=Some(ctc (A, Le, C))};
    {l=And(ctc (A, Eq, B), ctc (C, Eq, D)); r=And(ctc (B, Eq, C), ctc (D, Eq, E)); expected=Some(And(ctc (A, Eq, C), And(ctc (B, Eq, D), ctc (C, Eq, E))))};
  ]

let cnfTest testCtx =
  let testCNF i args =
    let actual = cnfTrans args.l args.r in
    let () = printHeader ~argPrinter:stringCNFArg ~resPrinter:stringCNFRes ~cmp:cnfOptEq "cnfTest" (i+1) args.l args.r args.expected actual in
    assert_equal ~printer:stringCNFRes ~cmp:cnfOptEq args.expected actual in
  List.iteri testCNF cnfTCs

let suite =
  "suite">:::
    ["compSuite">:::
      ["compTest">:: compTest
      ]
    ;"predSuite">:::
      ["predTestWithComps">:: predTestWithComps
      ;"predTest">:: predTest
      ]
    ;"cnfSuite">:::
      ["cnfTestWithPreds">:: cnfTestWithPreds
      ;"cnfTest">:: cnfTest
      ]
    ]

let () =
  run_test_tt_main suite
