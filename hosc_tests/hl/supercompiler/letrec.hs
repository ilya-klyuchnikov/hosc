data List a = Nil  | Cons a (List a);

letrec f = \p -> 
	case p of {
		Nil -> y; 
		Cons u w -> Cons u (f w);
	} 
in f x