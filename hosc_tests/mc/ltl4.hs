{-# LANGUAGE NoImplicitPrelude#-}

data List a = Cons a (List a);
data Bool = True | False;

--X kP
--kX return kP states 

-- not kP
--kFnot return kP states 
--X (not kP)
--kFnot return (kX kP)  states 

--kX1 (kFnot1 kP) return states

--kFnot1 (kFnot1 (kX1 kP)) return states

--kFor1 (kFnot1 kP) (kFnot1 kP) return states
--kFand1 kP (kFnot1 kP) return states

kFand1 s (kFnot1 (kFnot1 s)) return states

where

kX = \k f ss -> ktail (f k) ss;
kFnot = \k f ss -> f (knot k) ss;

kX1 = \f k ss -> ktail (f k) ss;
kFnot1 = \f k ss -> f (knot k) ss;

kFor1 = \f1 f2 k ss -> 
	f1 (\v -> case v of {True -> k True; False -> f2 k ss;}) ss;
kFand1 = \f1 f2 k ss -> 
	f1 (\v -> case v of {True -> (\z -> f2 k ss) v; False -> k v;}) ss;
kU1 = \f1 f2 k ss -> kFor1 f1 (kFand1 f1 (kX1 (kU1 f1 f2))) k ss;

xNotF = \k-> kX k (\z -> (kFnot z kP));

kand = \k v1 v2 -> case v1 of {
	True -> k v2;
	False -> k False;
};
knot = \k v -> case v of {
	True -> k False;
	False -> k True;
};
kor = \k v1 v2 -> case v1 of {
	True -> k True;
	False -> k v2;
};
-- base 
kP = \k ls -> case ls of {
	Cons x xs -> k x; 
};
ktail = \k ls -> case ls of {Cons l1 ls1 -> k ls1;};

return = \x -> x;