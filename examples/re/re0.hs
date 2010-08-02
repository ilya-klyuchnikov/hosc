data Alphabet = A | B deriving Show;
data List a = Nil | Cons a (List a) deriving Show; 

type Word = List Alphabet;
type Parser = Word -> List Word;

orAB = \w -> match (orP a b) w;

match :: Parser -> Word -> Bool;
match = \p w -> case (p w) of {
	Nil -> False;
	Cons w1 ws -> True;
};

allWordsA = Cons Nil (mapL (append (Cons A Nil)) allWordsA);
allWordsAB = Cons Nil (merge (mapL (append (Cons A Nil)) (allWordsAB)) (mapL (append (Cons B Nil)) (allWordsAB)));

---------------
-- combinators
---------------
orP :: Parser -> Parser -> Parser;
orP = \p1 p2 w -> merge (p1 w) (p2 w);

conP :: Parser -> Parser -> Parser;
conP = \p1 p2 w -> join (mapL p2 (p1 w));

repP :: Parser -> Parser;
repP = \p w -> orP nilP (conP p (repP p)) w;
repK = \p w -> Cons w (join (mapL (repK p) (p w)));

--------------
-- parsers
--------------
a :: Parser;
a = \w -> case w of {
	Nil -> Nil;
	Cons l w1 -> case l of { A -> Cons w1 Nil; B -> Nil;};
};

b :: Parser;
b = \w -> case w of {
	Nil -> Nil;
	Cons l w1 -> case l of {A -> Nil; B -> Cons w1 Nil;};
};

nilP :: Parser;
nilP = \w -> Cons w Nil;

eow :: Parser;
eow = \w -> case w of {
	Nil -> Cons Nil Nil;
	Cons l w1 -> Nil;
};

---------------
-- list utility
---------------

merge :: List a -> List a -> List a;
merge = \ws1 ws2 -> case ws1 of {
	Nil -> ws2;
	Cons w1 nws1 -> case ws2 of {
		Nil -> ws1;
		Cons w2 nws2 -> Cons w1 (Cons w2 (merge nws1 nws2));
	};
};

append :: List a -> List a -> List a;
append = \xs ys -> case xs of {
	Nil -> ys;
	Cons x1 xs1 -> Cons x1 (append xs1 ys);
};

join :: List (List a) -> List a;
join = \xs -> case xs of { 
	Nil -> Nil;
	Cons x1 xs1 -> append x1 (join xs1);
};

mapL :: (t -> a) -> List t -> List a;
mapL = \f xs -> case xs of {
	Nil -> Nil;
	Cons x1 xs1 -> Cons (f x1) (mapL f xs1);
};
