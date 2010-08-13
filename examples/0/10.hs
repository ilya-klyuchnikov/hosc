-- good: HOSC, NaiveWithCC

data Nat = Z | S Nat;

fix (\x -> s x)

where

fix = \f -> f (fix f);
s = \n -> S n;
