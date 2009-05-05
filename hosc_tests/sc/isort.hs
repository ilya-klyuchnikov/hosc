// Program Optimizations and Transformations in Calculation Form

data List a = Nil | Cons a (List a);
data B = True | False;

isort pr xs

//insert pr x (isort pr xs)

where

isort = \pred xs ->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> insert pred x1 (isort pred xs1);
	};

insert = \pred x xs ->
	case xs of {
		Nil -> Cons x Nil;
		Cons x1 xs1 ->
			case pred x x1 of {
				True -> Cons x (Cons x1 xs1);
				False -> Cons x1 (insert pred x xs1); 
			};
	};