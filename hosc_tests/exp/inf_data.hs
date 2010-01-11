data List a = Nil | Cons a (List a);
data Bool = True | False;

f where

f = Cons True f;