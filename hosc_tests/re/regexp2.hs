{-# LANGUAGE NoImplicitPrelude#-}

data Alphabet = A | B ;
data List a = Nil | Cons a (List a) ;
data Bool = True | False ;
data Result a = Fail | Parsed a ; 

a w

where

match = \p w -> concat p eow return False False w;

a = \next f1 f w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> next False False w1; B -> Fail;};
};
b = \next f1 f w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> Fail; B -> next False False w1;};
};
nil = \next f1 f w -> case f1 of {
	True -> Fail;
	False -> next False f w;
};
eow = \next f1 f w -> case w of {
	Cons l w1 -> Fail;
	Nil -> case f1 of {
		True -> Fail;
		False -> next False f Nil;
	};
};

or = \p1 p2 next f1 f w -> case p1 next f1 f w of {
	Parsed w1 -> Parsed w1;
	Fail -> p2 next f1 f w;
};

return = \f1 f y -> case f of {
	False -> Parsed y;
	True -> Fail;
};
-- it seems now a bit tricky?
concat = \p1 p2 next f1 f w -> case f1 of {
	False -> p1 (p2 next) f1 f w;
	True -> concat1 p1 p2 next w;
};

concat1 = \p1 p2 next w -> 
	case p1 (p2 next) True True w of {
		Parsed y -> Parsed y;
		-- now we check whether p1 is nullable!
		Fail -> case p1 return False False Nil of {
			Fail -> Fail;
			Parsed y1 -> p2 next True True w; 
		};
	}; 

-- not necessary to eat
rep0 = \p next f w -> case next False f w of {
	Parsed y -> Parsed y;
	-- this is wrong!
	Fail -> concat p (rep p) next True True w;
};
rep = \p next f1 f w -> case f1 of {
	True -> p (rep p next) True True w;
	False -> rep0 p next f w;
};