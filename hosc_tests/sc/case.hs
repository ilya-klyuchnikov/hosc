data Num = Z | S Num;

g (\x -> case x of {
    Z -> r;
    S y -> m ;
}) t

where

f1 = \x -> S x;

g1 = \x ->
  case f1 x of {
    Z -> Z;
    S y -> g2 (g1 y);
  };

g2 = \x ->
  case f1 x of {
    Z -> Z;
    S y -> g1 x;
  };



g = \x -> g (g x);