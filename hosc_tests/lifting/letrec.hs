data List a = Nil  | Cons a (List a);
// app x (app y z)
letrec
  f=\w1 -> case w1 of {
    Nil -> (letrec g=\p1 -> case p1 of { Nil -> z; Cons y1 t -> (Cons y1 (g t)); } in (g y));
    Cons s u1 -> (Cons s (f u1));
  }
in
  f x