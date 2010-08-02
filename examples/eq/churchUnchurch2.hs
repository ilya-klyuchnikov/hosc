data Nat = Z | S Nat;

idNat m where

idNat = \n -> case n of { Z -> Z; S n1 -> S (idNat n1); };

church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};

unchurch = \n -> n (\x -> S x) Z;