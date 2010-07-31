data List a = Nil  | Cons a (List a);

letrec
  app3 = \p -> case p of {
    Nil -> letrec 
    		app2 = \q -> 
    			case q of { 
    				Nil -> z; 
    				Cons v w -> Cons v (app2 w);
    			} 
    		in app2 y;
    Cons v w -> Cons v (app3 w);
  }
in app3 x