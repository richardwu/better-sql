let q = SELECT(
  PROJECT(
    Compare(Eq, VAR(Table "a"), CONST(1)),
    JOIN(
      Inner, Compare(Eq, VAR(Table "a"), VAR(Table "b")), SCAN(Table "a"), SCAN(Table "b")
    )
  )
)

let qpushed = pushdown q None
