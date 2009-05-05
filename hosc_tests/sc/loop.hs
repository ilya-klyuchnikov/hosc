data List a = Nil | Cons a (List a);
data Boolean = True | False;

f x where

f = \x -> f x;