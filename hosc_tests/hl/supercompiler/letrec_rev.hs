data List a = Nil  | Cons a (List a);

// rev zs
letrec
  f=\v1 -> case v1 of {
    Nil -> Nil;
    Cons u y -> (letrec g=\w1 -> 
    	case w1 of { 
    		Cons w z1 -> (Cons w (g z1)); 
    		Nil -> (Cons u Nil); 
    	} 
    	in (g (f y)));
  }
in
  f zs