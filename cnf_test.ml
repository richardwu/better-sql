open Cnf

type test =
  | A
  | B
  | C
  | D

let () = assert ((compTrans (A, Eq, B) (B, Lt, C)) = Some(A, Lt, C))

let () = assert ((compTrans (A, Lt, B) (C, Gt, B)) = Some(A, Lt, C))

let () = assert ((predTrans (Comp(A, Eq, B)) (Not(Comp(B, Lt, C)))) = (Some(Not(Comp(A, Lt, C)))))
