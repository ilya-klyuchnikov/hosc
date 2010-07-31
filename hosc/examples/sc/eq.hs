data Number = Z | S Number;
data Boolean = True | False;

eq x x

where

eq = \x y -> case x of {Z -> eqZ y; S x1 -> eqS y x1;};

eqZ = \x -> case x of {Z -> True; S x1 -> False;};

eqS = \y x ->  case y of {Z -> False; S y1 -> eq x y1;};