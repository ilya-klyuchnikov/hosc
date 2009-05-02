data List a = Nil | Cons a (List a);

case (app r (Cons p Nil)) of {
    Nil -> ps;
    Cons v vs -> Cons v (app vs ps);
}

where

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };