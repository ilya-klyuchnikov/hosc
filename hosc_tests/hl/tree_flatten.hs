data Tree a = Leaf a | Node (Tree a) (Tree a);
data List a = Nil | Cons a (List a);

flatten

where

append = \xs ys -> case xs of {
  Nil -> ys;
  Cons x xs1 -> Cons x (append xs1 ys);
  };

flatten = \t -> case t of {
  Leaf x -> Cons x Nil;
  Node x y -> append (flatten x) (flatten y);
  };