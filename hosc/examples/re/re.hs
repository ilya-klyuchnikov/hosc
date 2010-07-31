{-# LANGUAGE NoImplicitPrelude#-}

data Alphabet = A | B;
data List a = Nil | Cons a; 
data Option a = None | Some a;

type Word a = List a;
type RE a b = (Option (List a) -> b) -> (List a) -> Option (List a);  

-- continuation!!!!
or = \re1 re2 k w = case re1 k w of {
	Some -> Some;
	None -> re2 k w;
};

concat = \re1 re2 k w -> case re1 (\x -> x) w of {
	Some w1 -> re2 k w; 
	None -> None;
};
 
rep = \re1 k w -> case k w of {
	Some w1 -> Some w1;
	None -> re1 (rep re1 k) w;
};

none = \k w -> k None;
nil = \k w -> case w of {
	Nil -> k (Some Nil);
	Cons l1 w1 -> k None;
};
a = \k w case w of {
	Nil -> k None;
	Cons l w1 -> case l of { A -> k (Some w1); B -> k None;};
};
b = \k w case w of {
	Nil -> k None;
	Cons l w1 -> case l of {A -> k None; B -> k (Some w1);};
};
