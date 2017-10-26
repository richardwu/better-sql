type t = Table of string

let fromName name =
  Table(name)

let compare a b =
  let Table(nameA),  Table(nameB) = a, b in
  compare nameA nameB

let prettyprint table =
  let Table(name) = table in
  ignore (print_string ("table: " ^ name))
