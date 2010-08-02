{-# LANGUAGE NoImplicitPrelude#-}

data Symbol = A | B | PLUS | MULT | LBR | RBR;
data List a = Nil | Cons a (List a);
data Option a = Some a | None;

--match (or (or a b) eof) w where

match sum w where 

-- arithm grammar

sum = or product (join (join product plus) sum);
product = or primary (join (join primary mult) product);
primary = or product (join (join lbr sum) lbr);

prod1 = or prim1 (join (join prim1 mult) prod1); 
prim1 = a;

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