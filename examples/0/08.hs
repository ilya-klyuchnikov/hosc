data List a = Nil | Cons a (List a);
data Nat = Z | S Nat;

map inc (map inc z)

where

map = \f xs ->
  case xs of {
    Nil         -> Nil;
    Cons x1 xs1 -> Cons (f x1) (map f xs1);
  };

inc = \n -> S n;