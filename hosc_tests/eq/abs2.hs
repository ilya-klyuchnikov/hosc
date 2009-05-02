data List a = Nil | Cons a (List a);

idList xs where

compose = \f g x ->  f (g x);

rep = \xs -> append xs;

append = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (append xs1 ys);
	};

abs = \f -> f Nil;

idList = \xs ->
   case xs of {
     Nil -> Nil;
     Cons x1 xs1 -> Cons x1 (idList xs1);
   };