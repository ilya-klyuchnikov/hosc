-- bad: Naive

data List a = Nil | Cons a (List a);
data Nat = Z | S Nat;

fold plus Z (map square xs) where

fold = \f c xs -> case xs of {
        Nil -> c;
        Cons y ys -> f y (fold f c ys);
};

map = \f xs -> case xs of {
        Nil -> Nil;
        Cons y ys -> Cons (f y) (map f ys);
};