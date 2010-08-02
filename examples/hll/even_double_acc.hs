{-# LANGUAGE NoImplicitPrelude#-}

data Number = Z | S Number;
data Boolean = True | False;

even (double n Z) where

even = \x ->
  case x of {
    Z -> True;
    S y -> case y of {
       Z -> False;
       S z -> even z;
    };
  };

double = \x y ->
  case x of {
    Z -> y;
    S z -> double z (S (S y));
  };