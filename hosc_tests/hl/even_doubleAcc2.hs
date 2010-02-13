data Bool = True | False;
data Nat = Z | S Nat;

case doubleAcc v (S (S Z)) of {Z  -> True; S w -> odd w;} 

where

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

doubleAcc = \x y -> case x of {
  Z -> y;
  S x1 -> doubleAcc x1 (S (S y));
};