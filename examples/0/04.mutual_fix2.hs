-- All except HOSC15 - bad results

data Input = A Input | B Input;
data Pair a b = P a b;

fst (fix (\r -> t2 (A (snd r)) (B (fst r))))

where

t2 = \x y f -> f x y;

fst = \u -> u (\x y -> x);
snd = \u -> u (\x y -> y);

fix = \f -> f(fix f);