// Distillation: Extracting the essence of programs

data N = Z | S N;
data B = True | False;

leq x (plus y x)
where

leq = \x y ->
  case x of {
    Z -> True;
    S x1 -> case y of {Z -> False; S y1 -> leq x1 y1;};
  };

plus = \x y ->
  case x of {Z -> y; S x1 -> S (plus x1 y);};