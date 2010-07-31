{-# LANGUAGE NoImplicitPrelude#-}

data Symbol = A | B;
data List a = Nil | Cons a (List a);
data Option a = Some a | None;


(join a (join doublea a)) (eof return) word

where 

match = \p i -> p (eof return) i;
return = \x -> Some x;

doublea = or nil (join a (join doublea a));

or = \p1 p2 next w -> case p1 next w of {
	Some w1 -> Some w1;
	None -> p2 next w;
};

nil = \next w -> next w;

join = \p1 p2 next w -> p1 (p2 next) w;

a = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> next w1;
		B -> None;
	};
	Nil -> None;
};

b = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> None;
		B -> next w1;
	};
	Nil -> None;
};

eof = \next w -> case w of {
	Cons s w1 -> None;
	Nil -> next Nil;
};