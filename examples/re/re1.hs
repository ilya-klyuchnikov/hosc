data Alphabet = A | B;
data List a = Nil | Cons a (List a); 
data Bool = True | False;

--fullMatch (orK a (repK a)) w

--fullMatch (repK a) w

--fullMatch (orK (repK a) a) w

--rev xs 

--allWordsAB Nil

match (orK y (repK y)) ws

where

allWordsA = \x -> Cons x (mapL (append (Cons A Nil)) (allWordsA x));
allWordsAB = Cons Nil (merge (mapL (append (Cons A Nil)) (allWordsAB)) (mapL (append (Cons B Nil)) (allWordsAB)));

allW = Cons Nil (g allW);
g= \ws -> case  ws  of {Nil  -> Nil; Cons s s2 -> (Cons (Cons A s) (Cons (Cons B s) (g s2)));};

orAB = \w -> match (orK a b) w;

fullMatch = \p w -> match (conK p eow) w;
match = \p w -> case (p w) of {
	Nil -> False;
	Cons w1 ws -> True;
};

---------------
-- combinators
---------------
orK = \p1 p2 w -> merge (p1 w) (p2 w);
conK = \p1 p2 w -> join (mapL p2 (p1 w));
repK = \p w -> orK nilP (conK p (repK p)) w;
--repK = \p w -> Cons w (join (mapL (repK p) (p w)));


--------------
-- parsers
--------------
a = \w -> case w of {
	Nil -> Nil;
	Cons l w1 -> case l of { A -> Cons w1 Nil; B -> Nil;};
};
b = \w -> case w of {
	Nil -> Nil;
	Cons l w1 -> case l of {A -> Nil; B -> Cons w1 Nil;};
};
nilP = \w -> Cons w Nil;
eow = \w -> case w of {
	Nil -> Cons Nil Nil;
	Cons l w1 -> Nil;
};

---------------
-- list utility
---------------
merge = \ws1 ws2 -> case ws1 of {
	Nil -> ws2;
	Cons w1 nws1 -> case ws2 of {
		Nil -> ws1;
		Cons w2 nws2 -> Cons w1 (Cons w2 (merge nws1 nws2));
	};
};

rev = \xs -> case xs of {
	Nil -> Nil;
	Cons x1 xs1 -> append (rev xs1) (Cons x1 Nil);
};

append = \xs ys -> case xs of {
	Nil -> ys;
	Cons x1 xs1 -> Cons x1 (append xs1 ys);
};

join = \xs -> case xs of { 
	Nil -> Nil;
	Cons x1 xs1 -> append x1 (join xs1);
};

mapL = \f xs -> case xs of {
	Nil -> Nil;
	Cons x1 xs1 -> Cons (f x1) (mapL f xs1);
};

filter = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 ->
      case p x of {
         True -> Cons x (filter p xs1);
         False -> filter p xs1;
      };
  };
