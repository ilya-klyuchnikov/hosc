data Nat = Z | S Nat;

idNat (pred x) where

idNat = \n -> case n of { Z -> Z; S n1 -> S (idNat n1); };

church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};

unchurch = \n -> n (\x -> S x) Z;

pred = \x -> case x of { Z -> Z; S x1 -> x1; };

churchPred = \n f x -> n (\g h -> h (g f)) (\u -> x) (\v -> v);