data List a = Nil | Cons a (List a);

x where

snocr = \ys y -> 
	case ys of {
		Nil -> Cons y Nil;
		Cons x xs -> Cons x (snocr xs y);
	};
	
foldr = \c h xs -> 
	case xs of {
		Nil -> c;
		Cons y ys -> h y (foldr c h ys); 
	};
	
foldl = \c h xs -> 
	case xs of {
		Nil -> c;
		Cons y ys -> foldl (h c y) h ys; 
	};
	
	
rev = \xs -> 
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> app (rev xs1) (Cons x1 Nil);
	};
	
rev1 = \xs ys -> 
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> rev1 xs1 (Cons x1 ys);
	};
	
append = \x xs -> snocr xs x;
prepend = \xs x -> Cons x xs;

app = \xs ys -> 
	case xs of {
		Nil -> ys; 
		Cons x1 xs1 -> Cons x1 (app xs1 ys); 
	};
	
map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> Cons (f x1) (map f xs1);
  };