data Bool = True | False;
data Nat = Z | S Nat;

--or (even n) (odd n)

case (even v29) of {True  -> (True); False  -> (odd v29);}

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

and = \x y ->
  case x of {
    True -> y;
    False -> False;
};

or = \x y ->
  case x of {
    True -> True;
    False -> y;
};