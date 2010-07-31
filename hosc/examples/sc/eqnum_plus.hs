f where

f = \x -> f (g x);
g = \x -> g (f x);