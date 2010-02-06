data List a = Nil | Cons a (List a);
data Boolean = True | False;
data Pair a b = P a b;
data Option a = None | Some a;

x where

compose = \f->\g->\x->f (g x);
unit = \x->Cons x Nil;
rep = \xs-> append xs;

abs = \f-> f Nil;
iterate = \f->\x-> Cons x (iterate f (f x));
fp = \p1->\p2->
	case p1 of {
		P a1 a2 -> 
			case p2 of { P b1 b2 -> P (a1 b1) (a2 b2);};
	};

map = \f->\xs->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> Cons (f x1) (map f xs1);
	};	

join = \xs->
	case xs of { 
		Nil -> Nil;
		Cons x1 xs1 -> append x1 (join xs1);
	};

append = \xs->\ys->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (append xs1 ys);
	};

idList = \xs->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> Cons x1 (idList xs1);
	};

filter = \p->\xs->
	case xs of {
		Nil -> Nil;
		Cons x xs1 -> 
			case p x of {
				True -> Cons x (filter p xs1);
				False -> filter p xs1;
			};
	};

zip = \p->
case p of { 
P xs ys ->
	case xs of {
		Nil -> Nil;
		Cons x1 xs1 -> 
                   case ys of { 
                      Nil -> Nil;
                      Cons y1 ys1 -> Cons (P x1 y1) (zip (P xs1 ys1));
                    };
     };
};