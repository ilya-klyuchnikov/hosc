data List a = Nil | Cons a (List a);
data Boolean = True | False;

app (Cons True Nil) (Cons False Nil)

where

app = \xs ys -> 
    case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };