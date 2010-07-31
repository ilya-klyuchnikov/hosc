data List a = Nil | Cons a (List a);

-- compose (map f) unit = compose unit f 
x where

compose = \f1 f2 x ->  f1 (f2 x);
unit = \x -> Cons x Nil; 

map = \f xs ->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> Cons (f x1) (map f xs1);
	};
