{-# LANGUAGE NoImplicitPrelude#-}

data Symbol = A;
data List a = Nil | Cons a (List a);
data Option a = Some a | None;


--match evena w

--((evena (eof return)) w)
((evena (a (eof return))) v33)
--((evena (a (a (eof return)))) (Cons v62 v63))
--((evena (a (a (a (eof return))))) (Cons A (Cons v136 v137)))

where

odda = or a (join a (join odda a));
evena = or nil (join a (join evena a));

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
	};
	Nil -> None;
};

nil = \next w -> next w;

eof = \next w -> case w of {
	Cons s w1 -> None;
	Nil -> next Nil;
};