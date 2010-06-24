{-# LANGUAGE NoImplicitPrelude#-}

data Symbol = A | B | MULT | LBR | RBR | PLUS;
data List a = Nil | Cons a (List a);
data Option a = Some a | None;



--match (join (join x y) z) w

match l1 w


where

prod1 = or prim1 (join prim1 (join mult prod1)); 
prim1 = or a braced;
braced = join lbr (join prod1 rbr);

l1 = or a b1;
b1 = join lbr (join l1 rbr); 

match = \p i -> p (eof return) i;
return = \x -> Some x;

or = \p1 p2 next w -> case p1 next w of {
	Some w1 -> Some w1;
	None -> p2 next w;
};

join = \p1 p2 next w -> p1 (p2 next) w;

a = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> next w1;
		B -> None;
		PLUS -> None;
		MULT -> None;
		LBR -> None;
		RBR -> None;
	};
	Nil -> None;
};

b = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> None;
		B -> next w1;
		PLUS -> None;
		MULT -> None;
		LBR -> None;
		RBR -> None;
	};
	Nil -> None;
};

plus = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> None;
		B -> None;
		PLUS -> next w1;
		MULT -> None;
		LBR -> None;
		RBR -> None;
	};
	Nil -> None;
};

mult = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> None;
		B -> None;
		PLUS -> None;
		MULT -> next w1;
		LBR -> None;
		RBR -> None;
	};
	Nil -> None;
};

lbr = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> None;
		B -> None;
		PLUS -> None;
		MULT -> None;
		LBR -> next w1;
		RBR -> None;
	};
	Nil -> None;
};

rbr = \next w -> case w of {
	Cons s w1 -> case s of {
		A -> None;
		B -> None;
		PLUS -> None;
		MULT -> None;
		LBR -> None;
		RBR -> next w1;
	};
	Nil -> None;
};

eof = \next w -> case w of {
	Cons s w1 -> None;
	Nil -> next Nil;
};