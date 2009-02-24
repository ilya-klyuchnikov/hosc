data List a = Nil  | Cons a (List a);

letrec
  f= \xs1 -> 
	  case xs1 of {
	    Nil -> letrec g= \ys1 -> 
	    		case u1 of { Nil -> zs; Cons y2 ys2 -> (Cons y2 (g ys2)); } 
	    		in g ys;
	    Cons x2 xs2 -> Cons x2 (f xs2);
	  }
in
  f xs