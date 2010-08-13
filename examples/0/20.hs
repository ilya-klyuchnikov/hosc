data List a = Nil | Cons a (List a);

f y where

f = \x -> case x of {
        Nil -> Nil;
        Cons y ys -> f (f ys);
};