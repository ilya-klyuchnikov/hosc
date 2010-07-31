{-# LANGUAGE NoImplicitPrelude#-}

data List a = Cons a (List a);
data Bool = True | False;

-- boolean formula -- list of single bools
type BF = (List Bool) -> Bool;
-- Continuation simple formula --monad???
type KBF k = (Bool -> k) -> List Bool -> k;
type KBOp k = KBF k -> KBF k;

kX :: (Bool -> k) -> KBF k -> (List Bool -> k);
kX = \k f ss -> ktail (f k) ss;

kFnot :: (Bool -> k) -> KBF k -> (List Bool -> k);
kFnot = \k f ss -> f (knot k) ss;

kX1 :: KBF k -> KBF k; 
kX1 = \f k ss -> ktail (f k) ss;
kFnot1 :: KBF k -> KBF k;
kFnot1 = \f k ss -> f (knot k) ss;

kFor1 :: KBF k -> KBF k -> KBF k;
--kFor1 = \f1 f2 k ss -> f1 (\v -> kor k v f2) ss;
kFor1 = \f1 f2 k ss -> 
	f1 (\v -> case v of {True -> k True; False -> f2 k ss;}) ss;

-- this is real functor!!! <f, g> <x, y> = <f x, y x>
kFor2 = \f1 f2 k ss -> 
	f1 (\v -> case v of {True -> k v; False -> (\z -> f2 k ss) v;}) ss;
	
kFand1 :: KBF k -> KBF k -> KBF k;
kFand1 = \f1 f2 k ss -> 
	f1 (\v -> case v of {True -> (\z -> f2 k ss) v; False -> k v;}) ss;

kU1 :: KBF k -> KBF k -> KBF k;
kU1 = \f1 f2 k ss -> kFor1 f1 (kFand1 f1 (kX1 (kU1 f1 f2))) k ss;

kF1 = \f1 k ss -> kFor1 f1 (kX1 (kF1 f1)) k ss;
kG1 = \f1 k ss -> kAnd1 f1 (kX1 (kG1 f1)) k ss;   

-- X (not kP)
xNotF = \k-> kX k (\z -> (kFnot z kP));
xNotF1 = kX1 (kFnot1 kP); 

ktail :: (List t -> t1) -> List t -> t1;
ktail = \k ls -> case ls of {Cons l1 ls1 -> k ls1;};

kand :: (Bool -> t) -> Bool -> Bool -> t;
kand = \k v1 v2 -> case v1 of {
	True -> k v2;
	False -> k False;
};

knot :: (Bool -> t) -> Bool -> t;
knot = \k v -> case v of {
	True -> k False;
	False -> k True;
};

kor :: (Bool -> t) -> Bool -> Bool -> t;
kor = \k v1 v2 -> case v1 of {
	True -> k True;
	False -> k v2;
};

-- kP :: (Bool -> k) -> List Bool -> k
kP :: KBF k;
kP = \k ls -> case ls of {
	Cons x xs -> k x; 
};
return = \x -> x;