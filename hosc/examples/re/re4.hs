{-# LANGUAGE NoImplicitPrelude#-}

--import Prelude(Show)

data Alphabet = A | B;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Option a = None | Some a; 

task7c w

where

-- a+ | nil == (a | nil)+
task1a = \w -> match (or (rep1 a) nilP) w;
task1b = \w -> match (rep1 (or a nilP)) w;

-- aa* | nil = a*
task2a = \w -> match (or (concat a (rep a)) nilP) w;
task2b = \w -> match (rep a) w;

-- a*a* == a*
task3a = \w -> match (concat (rep a) (rep a)) w;
task3b = \w -> match (rep a) w;

-- (a | nil) a* == a*
task4a = \w -> match (concat (or a nilP) (rep a)) w;
task4b = \w -> match (rep a) w;

-- (a | nil)* == a*
task5a = \w -> match (rep (or a nilP)) w;
task5b = \w -> match (rep a) w;

-- a a* == a* a
task6a = \w -> match (concat a (rep a)) w;
task6b = \w -> match (concat (rep a) a) w;

-- (a+b)* == (a*b*)*
task7a = \w -> match (rep (or a b)) w;
task7b = \w -> match (rep1 (concat (rep1 a) (rep1 b))) w;
task7c = \w -> match (rep1 (concat (rep0 a) (rep0 a))) w;

match = \p w -> concat p eow return w;

concat = \p1 p2 k w -> p1 (p2 k) w;

or = \p1 p2 k w -> case p1 k w of {
	Some w1 -> Some w1;
	None -> p2 k w;
};
rep = \p next w -> case next w of {
	None -> p (rep p next) w;
	Some w1 -> Some w1; 
};
-- whether next or P include Nil 
{--
rep0 = \p next w -> 
	case w of {
		Nil -> next w;
		Cons l w1 -> case next Nil of {
			None -> concat (p notEmpty) (rep0 p) next w;
			Some w2 -> case next w of {
				None -> concat (p notEmpty) (rep0 p) next w;
				-- next is interesting!
				Some w3 -> 
			};
		}; 
	};
--}

rep1 = \p next w -> case w of {
	Nil -> None;
	Cons x w1 -> or p (concat p (rep1 p)) next w;
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
notEmpty = \next w -> case w of {
	Nil -> None;
	Cons l w1 -> Some w;
};