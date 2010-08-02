-- Program Optimizations and Transformations in Calculation Form

data List a = Nil | Cons a (List a);
data B = True | False;
data N = Z | S N;

-- note that hd < isort (homeomorphic embedding!)

--hd z (isort lessOrEq xs)

{-
hd z (isort lessOrEq (Cons x xs)) // this will be the minimal element

hd z (insert lessOrEq x xs)

hd z (insert lessOrEq x (isort lessOrEq xs))

-}

case (case ((((insert lessOrEq) w42) ((isort lessOrEq) w43))) of {Nil  -> (Cons x (Nil )); Cons w11 w12 -> case (((lessOrEq x) w11)) of {True  -> (Cons x (Cons w11 w12)); False  -> (Cons w11 (((insert lessOrEq) x) w12));};}) of {Nil  -> z; Cons w3 w4 -> w3;}

where

hd = \x xs -> 
	case xs of {
		Nil-> x;
		Cons x1 xs1-> x1;
	};

isort = \lessOrEq xs ->
	case xs of {
		Nil-> Nil;
		Cons x1 xs1-> insert lessOrEq x1 (isort lessOrEq xs1);
	};

insert = \lessOrEq x xs ->
	case xs of {
		Nil -> Cons x Nil;
		Cons x1 xs1 ->
			case lessOrEq x x1 of {
				True -> Cons x (Cons x1 xs1);
				False -> Cons x1 (insert lessOrEq x xs1); 
			};
	};

min = \z xs f ->
    case xs of {
        Nil -> z;
        Cons x1 xs1 -> case f x1 (min z xs1 f) of {True -> x1; False -> min z xs1 f; };
	};