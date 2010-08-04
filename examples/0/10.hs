-- good: HOSC, NaiveWithCC

data Bool = True | False;
data Nat = Z | S Nat;

fix (\x -> natS x)

where

fix = \f -> f (fix f);
natS = \n -> S n;
