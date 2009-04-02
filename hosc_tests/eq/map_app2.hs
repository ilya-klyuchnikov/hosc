data List a = Nil | Cons a (List a);

append (map f xs) (map f ys)

where

map = \f xs ->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> Cons (f x1) (map f xs1);
	};

append = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (append xs1 ys);
	};