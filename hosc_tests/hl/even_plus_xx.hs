data Bool = True | False;
data Nat = Z | S Nat;

even (plus x x) where

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

plus = \x y ->
    case x of {
      Z   -> y;
      S n -> S (plus n y);
    };