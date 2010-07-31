{-# LANGUAGE NoImplicitPrelude#-}
import Prelude(Show)

data Nat = Z | S Nat deriving Show;
data Boolean = False | True deriving Show;

--eq (add (unchurch x) (unchurch y)) (unchurch (churchAdd y x))

--eq (add (unchurch x) (unchurch x)) (unchurch (churchAdd x x))

--where

eq = \x y -> case x of {
  Z    -> case y of {Z -> True;  S y1 -> False;   } ;
  S x1 -> case y of {Z -> False; S y1 -> eq x1 y1;} ;
};
church = \n -> case n of {
  Z    -> \f x -> x;
  S n1 -> \f x -> f (church n1 f x);
};
unchurch = \n -> n s Z;
s = \x -> S x;
churchAdd = \m n -> (\f x -> m f (n f x));
add = \x y -> case x of {
  Z -> y;
  S x1 -> S (add x1 y);
};
sx = \f n -> case n of {
	S n1 -> Z;
	Z -> S Z;
};