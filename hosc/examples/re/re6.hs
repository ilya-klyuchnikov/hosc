{-# LANGUAGE NoImplicitPrelude#-}


data Alphabet = A | B | C;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Result a = Fail | Parsed a; 

match (concat1 (rep (concat1 (rep a) (rep b))) (or a b)) (Cons v1 Nil)

where

-- concat1 = \p1 p2 next force w -> p1 (p2 next) force w;
match = \p w -> concat1 p eow return False w;

--match = \p w -> p (eow return) False w;

-- nil | a*  == a*
-- DONE
task1a = \w -> match (or nil1 (rep1 a)) w;
task1b = \w -> match (rep1 (or nil1 a)) w;
task1c = \w -> match (or (rep1 a) nil1) w;
task1e = \w -> match (rep1 a) w;

-- aa* | nil == a*
-- DONE
task2a = \w -> match (or (concat1 a (rep1 a)) nil1) w;
task2b = \w -> match (or nil1 (concat1 a (rep1 a))) w;
task2c = \w -> match (rep1 a) w;

-- a*a* == a*
-- DONE
task3a = \w -> match (concat1 (rep1 a) (rep1 a)) w;
task3b = \w -> match (rep1 a) w;

-- (nil | a) a* == a*
-- DO NOT WORK because of Nil
task4a = \w -> match (concat1 (or1 nil1 a) (rep a)) w;
task4b = \w -> match (rep a) w;

-- a(ba)* == (ab)*a
-- DONE
task5a = \w -> match (concat1 a (rep1 (concat1 b a))) w;
task5b = \w -> match (concat1 (rep1 (concat1 a b)) a) w;

-- (a*b)*a* == a*(ba*)*
-- A -> Doesn't work (why?)
task6a = \w -> match (concat (rep1 (concat1 (rep1 a) b)) (rep1 a)) w;
task6b = \w -> match (concat (rep a) (rep (concat b (rep a)))) w; 

-- (a+b)* == (a*b*)*
-- DONE - note rep instead of rep1 in b
task7a = \w -> match (rep1 (or a b)) w;
task7b = \w -> match (rep1 (concat1 (rep a) (rep b))) w;  

-- (a | b) (rep a) (rep b) = (a (rep a) (rep b)) | (b (rep a) (rep b))
-- DONE
task8a = \w -> match (concat1 (concat1 (or a b) (rep1 a)) (rep1 b)) w;
task8b = \w -> match (or (concat1 b (concat1 (rep1 a) (rep1 b))) (concat1 a (concat1 (rep1 a) (rep1 b)))) w;

-- (a|b) (a*b*)* = (a*b*)*(a|b)
-- a with control 
-- b -- without
-- See notes
task9a = \w -> match (concat1 (or a b) (rep1 (concat1 (rep a) (rep b)))) w;
task9b = \w -> match (concat (rep (concat (rep a) (rep b))) (or a b)) w;


a = \next force w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> next False w1; B -> Fail; C -> Fail;};
};

b = \next force w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> Fail; B -> next False w1; C -> Fail;};
};

c = \next force w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> Fail; B -> Fail; C -> next False w1;};
};

or = \p1 p2 next force w -> case p1 next force w of {
	Parsed w1 -> Parsed w1;
	Fail -> p2 next force w;
};

or1 = \p1 p2 next force w -> case p1 next force w of {
	Parsed w1 -> case p2 next force w of {
		Fail -> Parsed w1;
		Parsed w2 -> Parsed w1;
	};
	Fail -> p2 next force w;
};

concat = \p1 p2 next force w -> case force of {
	False -> p1 (p2 next) force w;
	True -> case (p1 return True w) of {
		Parsed w2 -> p2 next False w2;
		Fail -> case (p1 return False w) of {
			Fail -> Fail;
			Parsed w3 -> p2 next True w3;
		};
	}; 
};

concat1 = \p1 p2 next force w -> p1 (p2 next) force w;

rep = \p next force w -> case next force w of {
	Parsed y -> Parsed y;
	Fail -> case (p return True w) of {
		Fail -> Fail;
		Parsed w1 -> rep p next False w1;
	};
};

rep1 = \p next force w -> case (p return True w) of {
	Parsed y -> rep1 p next False y;
	Fail -> next force w;	
};

return = \force y -> case force of {
	False -> Parsed y;
	True -> Fail;
};

nil = \next force w -> case force of {
	True -> Fail;
	False -> next False w;
};

nil1 = \next force w -> next force w;

eow = \next force w -> case force of {
	True -> Fail;
	False -> case w of {
		Nil -> next False Nil;
		Cons l w1 -> Fail;
	};
};

eow1 = \next force w -> case w of {
	Nil -> case force of {
			True -> Fail;
			False -> next False Nil;
		};
		Cons l w1 -> Fail;
	};
	
eow2 = \next force w -> case w of {
	Cons l w1 -> Fail;
	Nil -> next force Nil;
};