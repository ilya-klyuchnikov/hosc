data List a = Nil | Cons a (List a);

--rev (map f xs)
rev

where

map = \f xs -> case xs of {
  Nil -> Nil;
  Cons x xs1 -> Cons (f x) (map f xs1);
  };

revAcc = \xs ys -> case xs of {
  Nil -> ys;
  Cons x xs1 -> revAcc xs1 (Cons x ys);
  };

rev = \xs -> revAcc xs Nil;