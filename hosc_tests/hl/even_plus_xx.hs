data Bool = T | F;
data Nat = Z | S Nat;

even (plus x x) where

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

plus = \x y ->
    case x of {
      Z   -> y;
      S n -> S (plus n y);
    };