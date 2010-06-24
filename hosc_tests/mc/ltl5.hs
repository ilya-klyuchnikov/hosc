{-# LANGUAGE NoImplicitPrelude#-}

data List a = Cons a (List a);
data Bool = True | False;

--(kFnot1 (kG1 (kFnot1 p))) return states

--kF1 (kFnot1 p) return states
kFnot1 (kG1 p) return states  

where


kFnot1 = \f k ss -> f (\v -> case v of {True -> k False; False -> k True;}) ss;
kFor1 = \f1 f2 k ss -> f1 (\v -> case v of {True -> k True; False -> f2 k ss;}) ss;
kFand1 = \f1 f2 k ss -> f1 (\v -> case v of {True -> (\z -> f2 k ss) v; False -> k v;}) ss;

kX1 = \f k ss -> ktail (f k) ss;
kU1 = \f1 f2 k ss -> kFor1 f1 (kFand1 f1 (kX1 (kU1 f1 f2))) k ss;
kF1 = \f1 k ss -> kFor1 f1 (kX1 (kF1 f1)) k ss;
kG1 = \f1 k ss -> kFand1 f1 (kX1 (kG1 f1)) k ss;

kP = \k ls -> case ls of {Cons x xs -> k x;};
ktail = \k ls -> case ls of {Cons l1 ls1 -> k ls1;};

return = \x -> case x of {
	True -> True;
	False -> False;
};