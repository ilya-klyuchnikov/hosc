data Tree a = Leaf a | Fork (Tree a) (Tree a);
data List a = Nil | Cons a (List a);

flattenAcc

where

append = \xs ys -> case xs of {
  Nil -> ys;
  Cons x xs1 -> Cons x (append xs1 ys);
  };

flatten = \t -> case t of {
  Leaf x -> Cons x Nil;
  Fork x y -> append (flatten x) (flatten y);
  };

flattenAcc = \t u -> append (flatten t) u;