-- good: HOSC

data Input = A Input | B Input;

fst (fix (\r -> pair (A (snd r)) (B (fst r))))

where

pair = \x y f -> f x y;
fst = \p -> p (\x y -> x);
snd = \p -> p (\x y -> y);
fix = \f -> f(fix f);