{-# LANGUAGE NoImplicitPrelude#-}

import Prelude(Show)

data Alphabet = A | B deriving Show;
data List a = Nil | Cons a (List a) deriving Show;
data Bool = True | False deriving Show;
data Result a = Fail | Parsed a deriving Show; 

match = \p w -> concat p eow return False w;

-- a* | nil == a*
task1a = \w -> match (or (rep a) nil) w;
task1b = \w -> match (rep (or a nil)) w;

-- aa* | nil == a*
task2a = \w -> match (or (concat a (rep a)) nil) w;
task2b = \w -> match (rep a) w;

-- a*a* == a*
task3a = \w -> match (concat (rep a) (rep a)) w;
task3b = \w -> match (rep a) w;

-- (a | nil) a* == a*
task4a = \w -> match (concat (or a nil) (rep a)) w;
task4b = \w -> match (rep a) w;

task7a = \w -> match (rep (or a b)) w;
task7b = \w -> match (rep (concat (rep a) (rep b))) w;  

-- combinator for A letter
a = \next force w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> next False w1; B -> Fail;};
};
-- combinator for B letter
b = \next force w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> Fail; B -> next False w1;};
};
-- or combinator
or = \p1 p2 next force w -> case p1 next force w of {
	Parsed w1 -> Parsed w1;
	Fail -> p2 next force w;
};
-- Kleene star combinator
rep = \p next force w -> case (p return True w) of {
	Parsed y -> rep p next False y;
	Fail -> next force w;	
};
-- empty word combinator
nil = \next force w -> next force w;
-- concatenation combinator
concat = \p1 p2 next force w -> p1 (p2 next) force w;
-- success combinator
return = \force y -> case force of {
	False -> Parsed y;
	True -> Fail;
};
-- end of word combinator
eow = \next force w -> case w of {
	Cons l w1 -> Fail;
	Nil -> next force Nil;
};