data Nat = Z | S Nat;

churchSub (church x) (church y) where

idNat = \n -> case n of { Z -> Z; S n1 -> S (idNat n1); };

church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};

unchurch = \n -> n (\x -> S x) Z;

cpred = \n f x -> n (\g h -> h (g f)) (\u -> x) (\v -> v);

churchAdd = \m n -> (\f x -> m f (n f x));

churchSub = \n m -> (m cpred) n;