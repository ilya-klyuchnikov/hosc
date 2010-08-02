data Bool = True | False;
data Nat = Z | S Nat;

case  
case  n1  of { S n2 -> ((doubleAcc n2) (S (S (S (S Z))))); Z  -> (S (S Z)); }  
of {
  S n3 -> (odd n3);
  Z  -> True;
} 

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