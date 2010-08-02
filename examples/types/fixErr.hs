data Nat = Z | S Nat;
data Boolean = False | True;
data Pair a b = P a b;

(fix (\r -> P (even (snd r x) )    (odd (fst r y) ) ))

where

fst = \u -> case u of { P v w -> v; };
snd = \u -> case u of { P s t -> t; };

even = \f x -> case x of { Z -> True; S x1 -> f x1;};
odd = \g y -> case y of  { Z -> False; S y1 -> g y1;};

fix = \f -> f (fix f);
p = \a b -> P a b;