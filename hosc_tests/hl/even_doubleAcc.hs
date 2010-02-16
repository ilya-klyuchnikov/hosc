data Bool = T | F;
data Nat = Z | S Nat;

even (dAcc x Z) where

even = \x ->
  case x of {
    Z -> T;
    S x1 -> odd x1;
};

odd = \x ->
  case x of {
    Z -> F;
    S x1 -> even x1;
};

dAcc = \x y -> case x of {
  Z -> y;
  S x1 -> dAcc x1 (S (S y));
};