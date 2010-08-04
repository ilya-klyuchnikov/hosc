-- good: HOSC

data List a = Nil | Cons a (List a);

map f (iterate f x) where

iterate = \f x -> Cons x (iterate f (f x));

map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> Cons (f x1) (map f xs1);
  };