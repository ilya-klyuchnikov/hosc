data List a = Nil  | Cons a (List a);

app r (Cons p ps)

where

rev = \xs -> 
	case xs of {Nil -> Nil; Cons x1 xs1 -> app (rev xs1) (Cons x1 Nil);};

app = \xs ys -> 
	case xs of {Nil -> ys; Cons x1 xs1 -> Cons x1 (app xs1 ys); };
