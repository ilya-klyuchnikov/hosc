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