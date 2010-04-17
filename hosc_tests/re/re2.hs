data Alphabet = A | B;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Option a = None | Some a; 

notEmpty ((or a a) w)

where

or = \p1 p2 w -> case p1 w of {
	None -> p2 w;
	Some w1 -> case p2 w of {
		None -> Some w1;
		Some w2 -> choice{Some w1; Some w2;};
	};
};

a = \w -> case w of {
	Nil -> None;
	Cons l w1 -> case l of { A -> Some w1; B -> None;};
};

b = \w -> case w of {
	Nil -> None;
	Cons l w1 -> case l of {A -> None; B -> Some w1;};
};

nilP = \w -> Some w;

eow = \w -> case w of {
	Nil -> Some Nil;
	Cons l w1 -> None;
};

notEmpty = \o -> case o of {
	Some x -> True;
	None -> False;
};