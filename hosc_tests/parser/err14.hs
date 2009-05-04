data List a = Nil | Cons a (List a);
data List2 a = Nil2 | Cons2 a (List2 a);

x where 

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons2 z zs -> Cons z zs;
    };