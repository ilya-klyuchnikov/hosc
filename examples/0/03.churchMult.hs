-- all except naive are good

data Nat = Z | S Nat;
data Boolean = False | True;

eq (mult x y) (unchurch(churchMult (church x) (church y))) 

where

eq = \x y -> case x of {
  Z    -> case y of {Z -> True;  S y1 -> False;   } ;
  S x1 -> case y of {Z -> False; S y1 -> eq x1 y1;} ;
};

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

churchMult = \m n f -> m (n f);

mult = \x y -> case x of {
  Z -> Z;
  S x1 -> add y (mult x1 y);
};