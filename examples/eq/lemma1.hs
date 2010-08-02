data List a = Nil | Cons a (List a);

app r (Cons p ps)
where

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };