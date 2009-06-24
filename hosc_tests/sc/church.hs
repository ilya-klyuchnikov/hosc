data Nat = Z | S Nat;

church (add x y) where

//churchAdd (church x) (church y) where

church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};

unchurch = \n -> n (\x -> S x) Z;

churchAdd = \m n -> (\f x -> m f (n f x));
add = \x y -> case x of {
  Z -> y;
  S x1 -> S (add x1 y);
};