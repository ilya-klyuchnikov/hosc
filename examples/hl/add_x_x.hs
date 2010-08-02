data Nat = Z | S Nat;

add a a

where

add = \x y ->
  case x of {
    Z -> y;
    S x1 -> S (add x1 y);
};
