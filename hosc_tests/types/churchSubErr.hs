data Nat = Z | S Nat;

err where

church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};
 
unchurch = \n -> n (\x -> S x) Z;

cpred = \n -> (\f x -> n (\g h -> h (g f)) (\u -> x) (\v -> v));

churchSub = \n m -> (m cpred) n;

err = \x y -> unchurch (churchSub (church x) (church y));