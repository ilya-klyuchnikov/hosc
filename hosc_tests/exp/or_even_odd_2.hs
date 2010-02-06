data Bool = True | False;
data Nat = Z | S Nat;

--case
--  case  n  of { S x -> (odd x); Z  -> True; }  
--of { False  -> (odd (S (S n))); True  -> True; }

case (case (v29) of {Z  -> (True); S v58 -> (odd v58);}) of {True  -> (True); False  -> (odd (S (S v29)));}

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

or = \x y ->
  case x of {
    True -> True;
    False -> y;
};