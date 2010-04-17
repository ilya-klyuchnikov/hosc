data Alphabet = A | B;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Option a = None | Some a; 

--notEmpty ((or a a) w)

--notEmpty ((concat a a) w)

--notEmpty ((concat (rep a) eow) w)

--notEmpty (a return) w

--(a return) w

--concat (or a b) eow return w

--concat a (concat (rep a) eow) return w

notEmpty (concat (rep a) (concat a eow) return w)

where

concat = \p1 p2 k w -> p1 (p2 k) w;

or = \p1 p2 k w -> case p1 k w of {
	None -> p2 k w;
	Some w1 -> case p2 k w of {
		None -> Some w1;
		Some w2 -> choice{Some w1; Some w2;};
	};
};
rep = \p1 next w -> case p1 return w of {
	None -> next w;
	Some w1 -> case next w of {
		None -> rep p1 next w1;
		Some w2 -> choice {Some w; rep p1 next w1;}; 
	};
};
a = \next w -> case w of {
	Nil -> None;
	Cons l w1 -> case l of { A -> next w1; B -> None;};
};
b = \next w -> case w of {
	Nil -> None;
	Cons l w1 -> case l of {A -> None; B -> next w1;};
};
nilP = \next w -> next w;
eow = \next w -> case w of {
	Nil -> next Nil;
	Cons l w1 -> None;
};
return = \y -> Some y;
notEmpty = \o -> case o of {
	Some x -> True;
	None -> False;
};