data Nat = Z | S Nat;

churchSub (church x) (church y) where

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
pred = \x -> case x of {
  Z -> Z;
  S x1 -> x1;
};

churchAdd = \m n -> (\f x -> m f (n f x));

tru = \t f -> t;
fls = \t f -> f;
pair = \f s b -> b f s;
fst = \p -> p tru;
snd = \p -> p fls;
churchPred = \m -> fst (m ss zz);
c0 = \s z -> z;
c1 = \s z -> s z;
zz = (\z-> ((z (\u-> (\y-> y))) (\x-> (\w-> w))));
ss = \p -> pair (snd p) (churchAdd c1 (snd p));

churchSub = \n m -> (m churchPred) n;