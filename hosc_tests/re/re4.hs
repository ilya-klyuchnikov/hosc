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

concat (rep (or a b)) eow return w

where

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