data List a = Nil | Cons a (List a);
data Boolean = True | False;
data Pair a b = P a b; 

ordered leq (isort leq xs)
where

and = \x y -> case x of {False -> False; True -> y;};

ordered = \leq xs -> case xs of {
  Nil -> True;
  Cons x1 xs1 -> case xs1 of {Nil -> True; Cons x2 xs2 -> and (leq x1 x2) (ordered leq xs1);};
};

isort = \leq xs ->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> insert leq x1 (isort leq xs1);
	};

insert = \leq x xs ->
	case xs of {
		Nil -> Cons x Nil;
		Cons x1 xs1 ->
			case leq x x1 of {
				True -> Cons x (Cons x1 xs1);
				False -> Cons x1 (insert leq x xs1); 
			};
	};

 