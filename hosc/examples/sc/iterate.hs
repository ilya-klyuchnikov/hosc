data List a = Nil | Cons a (List a);
data Nat = Z | S Nat;

iterate (\n -> S n) z

where

iterate = \f x -> Cons x (iterate f (f x));