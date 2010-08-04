-- Free theorem 2
-- good: only HOSC

data List a = Nil | Cons a (List a);
data Pair a b = P a b;

map (fp (P f1 f2)) (zip (P l1 l2))

where

map = \f xs ->
        case xs of {
                Nil -> Nil;
                Cons x1 xs1 -> Cons (f x1) (map f xs1);
        };
    
zip = \p -> case p of {
   P xs ys ->
        case xs of {
                Nil -> Nil;
                Cons x1 xs1 -> 
                   case ys of {
                      Nil -> Nil;
                      Cons y1 ys1 -> Cons (P x1 y1) (zip (P xs1 ys1));
                   };
        };
};


fp = \p1 p2 -> case p1 of {P a1 a2 -> case p2 of {P b1 b2 -> P (a1 b1) (a2 b2); };};