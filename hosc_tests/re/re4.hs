data Alphabet = A | B;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Option a = None | Some a; 

--concat (concat (or a b) (or a b)) eow return w
--concat (concat a (rep a)) eow return w
--concat (concat (rep a) a) eow return w
--concat (concat (rep a) (rep a)) eow return w
--rep a (concat (rep a) eow return) w 
--rep a (rep a (eow return)) w

--concat (rep a) (concat (rep a) eow) return w

{-
case 
case  case  v26  of { Cons v60 v61 -> None; Nil  -> (return Nil); }  of {
  None  -> ((a ((rep a) (eow return))) v26);
  Some v62 -> (Some v62);
}
 of {
  None  -> ((a ((rep a) (((concat (rep a)) eow) return))) (Cons A v26));
  Some v63 -> (Some v63);
}
-}

-- (a+b)* == (a*b*)* .

--concat (rep (or a b)) eow return w

--match (rep (or a a)) w

task7a w

where

-- a* | nil == a*
task1a =\w -> match (concat (rep a) nilP) w;
task1b =\w -> match (rep a) w;

-- aa* | nil = a*
task2a = \w -> match (or (concat a (rep a)) nilP) w;
task2b = \w -> match (rep a) w;

-- a*a* == a*
task3a = \w -> match (concat (rep a) (rep a)) w;
task3b = \w -> match (rep a) w;

-- (a | nil) a* == a*
task4a = \w -> match (concat (or a nilP) (rep a)) w;
task4b = \w -> match (rep a) w;

-- (a | nil)* == a*
task5a = \w -> match (rep (or a nilP)) w;
task5b = \w -> match (rep a) w;

-- a a* == a* a
task6a = \w -> match (concat a (rep a)) w;
task6b = \w -> match (concat (rep a) a) w;

-- (a+b)* == (a*b*)*
task7a = \w -> match (rep (or a b)) w;
task7b = \w -> match (rep (concat (rep a) (rep b))) w;

match = \p w -> concat p eow return w;

concat = \p1 p2 k w -> p1 (p2 k) w;

or = \p1 p2 k w -> case p1 k w of {
	None -> p2 k w;
	Some w1 -> Some w1;
};
rep = \p next w -> case next w of {
	None -> p (rep p next) w;
	Some w1 -> Some w1; 
};
a = \next w -> case w of {
	Nil -> None;
	Cons l w1 -> case l of { A -> next w1; B -> None;};
};
b = \next w -> case w of {
	Nil -> None;
	Cons l w1 -> case l of {A -> None; B -> next w1;};
};
nilP = \next w -> next w;
eow = \next w -> case w of {
	Nil -> next Nil;
	Cons l w1 -> None;
};
return = \y -> Some y;
notEmpty = \o -> case o of {
	Some x -> True;
	None -> False;
};