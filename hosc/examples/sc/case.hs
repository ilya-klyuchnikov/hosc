data Num = Z | S Num;

g (\x -> case x of {
    Z -> r;
    S y -> m ;
}) t

where

g = \x -> g (g x);