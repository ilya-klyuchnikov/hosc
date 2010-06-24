{-# LANGUAGE NoImplicitPrelude#-}


-- Problems:
-- 1) forceEat doesn't look ahead -> so it result in only one variant from many
-- 2) without forceEat rep x --> x (rep x) -> the problem of nil combinator

data Alphabet = A | B;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Result a = Fail | Parsed a; 

--rep (concat b (rep a)) (eow return) False (Cons B (Cons A Nil))

(concat b (rep a)) return True (Cons B (Cons A Nil))
rep (or nil a) (Cons A Nil) 


where

match = \p w -> concat p eow return False w;

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
{-
-- or combinator
or = \p1 p2 next force w -> case p1 next force w of {
	Parsed w1 -> Parsed w1;
	Fail -> p2 next force w;
};
rep = \p next force w -> case next force w of {
	Parsed y -> Parsed y;
	Fail -> (forceEat p) (rep p next) False w;
};
forceEat = \p next force w -> case (p return True w) of {
	Fail -> Fail;
	Parsed y -> next False y;
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
-}
-- or combinator
or = \p1 p2 next force w -> case p1 next force w of {
	Parsed w1 -> Parsed w1;
	Fail -> p2 next force w;
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

rep = \p next force w -> case next force w of {
	Parsed y -> Parsed y;
	Fail -> (forceEat p) (rep p next) False w;
};

forceEat = \p next force w -> case (p return True w) of {
	Fail -> Fail;
	Parsed y -> next False y;
};