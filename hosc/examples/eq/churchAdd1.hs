data Nat = Z | S Nat;

unchurch(churchAdd (church x) (church y)) where

idNat = \n -> case n of { Z -> Z; S n1 -> S (idNat n1); };

church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};

unchurch = \n -> n (\x -> S x) Z;

add = \x y -> case x of {
  Z -> y;
  S x1 -> S (add x1 y);
};

churchAdd = \m n -> (\f x -> m f (n f x));