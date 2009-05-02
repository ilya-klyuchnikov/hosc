data List a = Nil | Cons a (List a);

rep (append xs ys) zs where

compose = \f g x ->  f (g x);

rep = \xs -> append xs;

append = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (append xs1 ys);
	};