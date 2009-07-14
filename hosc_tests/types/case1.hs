data Nat = Z | S Nat;
data Boolean = False | True;
data Pair a b = P a b;

fst

where

fst = \u -> case u of { P v w -> v; };
snd = \u -> case u of { P s t -> t; };