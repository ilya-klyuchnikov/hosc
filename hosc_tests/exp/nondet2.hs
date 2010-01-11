data N = Z | S N;


--f (f n) where

applyN (f Z) f Z where

f = \x -> choice {Z; S (f x);};

applyN = \n g x -> case n of {Z -> x; S n1 -> f (applyN n1 g x);}; 