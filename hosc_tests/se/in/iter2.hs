data List a = Nil | Cons a (List a);

//(compose join (map (map f))) xs where
case  ((append v20) (join v21))  of { Cons v10 v11 -> (Cons (f v10) ((map f) v11)); Nil  -> Nil; } where

compose = \f g x ->  f (g x);

map = \f xs ->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> Cons (f x1) (map f xs1);
	};

join = \xs ->
	case xs of { 
		Nil -> Nil;
		Cons x1 xs1 -> append x1 (join xs1);
	};

append = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (append xs1 ys);
	};