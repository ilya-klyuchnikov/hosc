{-# LANGUAGE NoImplicitPrelude#-}
data Nat = Z | S Nat;
data Boolean = False | True;
data Pair a b = P a b;



--(  (\fix1 f -> f (fix1 f)) (\r -> P (even (snd r) )    (odd (fst r) ) )) where

even = \f x -> case x of { Z -> True; S x1 -> f x1;};
odd = \g y -> case y of  {Z -> False; S y1 -> g y1;};
fix = \f -> f (fix f);
fst = \u -> case u of { P v w -> v; };
snd = \u -> case u of { P s t -> t; };