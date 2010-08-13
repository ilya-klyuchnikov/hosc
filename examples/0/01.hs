-- good: HOSC

data Input = A Input | B Input;
data Pair a b = P a b;

fst (fix (\r -> P (A (snd r)) (B (fst r))))

where

fst = \u -> case u of { P x y -> x; };
snd = \u -> case u of { P x y -> y; };

fix = \f -> f (fix f);