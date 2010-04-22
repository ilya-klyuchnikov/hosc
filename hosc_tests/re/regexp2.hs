{-# LANGUAGE NoImplicitPrelude#-}

data Alphabet = A | B ;
data List a = Nil | Cons a (List a) ;
data Bool = True | False ;
data Result a = Fail | Parsed a ; 

match (rep (rep a)) w

where

match = \p w -> p eow False False w;
a = \next f1 f w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> next False False w1; B -> Fail;};
};
b = \next f1 f w -> case w of {
	Nil -> Fail;
	Cons l w1 -> case l of { A -> Fail; B -> next False False w1;};
};
eow = \f1 f w -> case w of {
	Cons l w1 -> Fail;
	Nil -> return f1 f Nil;		
};
nil = \next f1 f w -> if f1 Fail (next False f w);

return = \f1 f y -> if f Fail (Parsed y);
concat = \p1 p2 next f1 f w -> if f1 (concatS p1 p2 next w) (concatS p1 p2 next w);
concatS = \p1 p2 next w -> or (\z -> p1 (p2 z)) (nullAnd p1 p2) next True True w;
nullAnd = \p1 p2 next f1 f2 w -> case p1 return False False Nil of {
	Fail -> Fail;
	Parsed y1 -> p2 next True True w;
};
rep0 = \p next f -> pipe (p (rep p next) True True) (next False f); 
rep = \p next f1 -> if f1 (p (rep p next) True) (rep0 p next);
or = \p1 p2 next f1 f -> pipe (p1 next f1 f) (p2 next f1 f);
pipe = \r1 r2 w -> case (r1 w) of { Parsed y -> Parsed y; Fail -> (r2 w);};
if = \cond a1 a2 -> case cond of {True -> a1; False -> a2;};