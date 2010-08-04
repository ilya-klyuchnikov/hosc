-- good: HOSC, Transient

data Nat = Z | S Nat;
data Boolean = False | True;

fst (fix (\r -> pair (even (snd r)) (odd (fst r)) ) ) x

where

fix = \f -> f (fix f);
even = \f x -> case x of { Z -> True; S x1 -> f x1;};
odd = \g y -> case y of  {Z -> False; S y1 -> g y1;};

pair = \x y z -> z x y;
fst = \p -> p (\x y -> x);
snd = \p -> p (\x y -> y);