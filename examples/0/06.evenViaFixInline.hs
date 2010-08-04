

data Nat = Z | S Nat;
data Boolean = False | True;
data Pair a b = P a b;

fst (fix (\r -> P ((\f x -> case x of { Z -> True; S x1 -> f x1;}) (snd r) )    ( (\g y -> case y of  {Z -> False; S y1 -> g y1;}) (fst r) ) ))

where

even = \f x -> case x of { Z -> True; S x1 -> f x1;};
odd = \g y -> case y of  {Z -> False; S y1 -> g y1;};
fix = \f -> f (fix f);
fst = \u -> case u of { P v w -> v; };
snd = \u -> case u of { P s t -> t; };