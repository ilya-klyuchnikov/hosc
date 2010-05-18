data List a = Nil | Cons a (List a);

iterate f (f x) where

iterate = \f x -> Cons x (iterate f (f x));