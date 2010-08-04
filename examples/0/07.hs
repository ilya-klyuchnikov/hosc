-- good: HOSC, Transient

data List a = Nil | Cons a (List a);
data Nat = Z | S Nat;
data Boolean = False | True;

map even (map double (iterate next Z))

where

next = \x -> S x;
iterate = \f x -> Cons x (iterate f (f x));

double = \x -> case x of {
  Z -> Z;
  S x1 -> S (S (double x1));
};

even = \x ->
  case x of {
    Z -> True;
    S x1 -> odd x1;
};

odd = \x ->
  case x of {
    Z -> False;
    S x1 -> even x1;
};

map = \f us -> case us of {
  Nil -> Nil;
  Cons x xs -> Cons (f x) (map f xs);
};