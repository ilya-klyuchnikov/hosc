data Nat = Z | S Nat;
data List a = Nil | Cons a (List a);


f1 x where

f1 = \x -> app (f2 (h x)) (f2 (add (h x) (S Z)));

f2 = \x -> app (f3 (h x)) (f3 (add (h x) (S Z)));

f3 = \x -> Cons (add x (S Z)) Nil;

h = \x -> mult (add x (S Z)) (S (S Z));

add = \x y -> case x of {
	Z -> y;
	S x1 -> S (add x1 y);
};

mult = \x y -> case x of {
  Z -> Z;
  S x1 -> add y (mult x1 y);
};

app = \xs ys -> case xs of {
	Nil -> ys;
	Cons x1 xs1 -> Cons x1 (app xs1 ys);
}; 