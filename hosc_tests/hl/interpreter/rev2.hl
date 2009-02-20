data List a = Nil | Cons a (List a);
data Boolean = True | False;

rev (Cons True (Cons False Nil))

where

rev = \xs ->
  case xs of {
    Nil -> Nil;
    Cons z  zs -> app (rev zs) (Cons z Nil);
  };

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };