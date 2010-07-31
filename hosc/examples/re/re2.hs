data Alphabet = A | B;
data List a = Nil | Cons a (List a);
data Bool = True | False;
data Option a = None | Some a; 

--notEmpty ((or a a) w)

--notEmpty ((concat a a) w)

--notEmpty ((concat (rep a) eow) w)

--notEmpty (a return) w

--(a return) w

--concat (or a b) eow return w

--concat a (concat a (concat (rep a) eow)) return w

--concat (rep a) (concat a eow) return w

--concat (concat (rep a) a) eow return w

--concat (rep a) (concat (rep a) eow) return w

--(concat (rep a) eow) return w

--or x x k w

--concat (rep (or a b)) eow return w

--rep (concat (rep a) (rep b)) return w

--concat (rep a) eow return w

--concat (concat (rep1 a) (rep1 a)) eow return w

concat (rep1 a) eow return w

where

f = \t47-> case  t47  of {
    Nil  -> Some Nil;
    Cons x4 t12 -> case  x4  of { 
    	A  -> case  (g t12)  of {
    		None  -> (f t12); 
    		Some y33 -> (Some (Cons A t12)); 
    	}; 
    	B  -> None; 
    };
};
g = \x48-> case  x48  of {
	Nil  -> Some Nil; 
	Cons s40 r28 -> case  s40  of {
		A  -> g r28; 
		B  -> None; 
	}; 
};

concat = \p1 p2 k w -> p1 (p2 k) w;

or = \p1 p2 k w -> case p1 k w of {
	None -> p2 k w;
	Some w1 -> case p2 k w of {
		None -> Some w1;
		Some w2 -> Some choice{w1; w2;};
	};
};

or1 = \p1 p2 k w ->
choice {
	case p1 k w of {
		None -> p2 k w;
		Some w1 -> case p2 k w of {
			None -> Some w1;
			Some w2 -> Some w1;
		};
	};
	case p1 k w of {
		None -> p2 k w;
		Some w1 -> case p2 k w of {
			None -> Some w1;
			Some w2 -> Some w2;
		};
	};
};

rep = \p1 next w -> case p1 return w of {
	None -> next w;
	Some w1 -> case next w of {
		None -> rep p1 next w1;
		Some w2 -> choice {Some w; rep p1 next w1;}; 
	};
};
rep1 = \p1 next w -> choice { 
	case p1 return w of {
		None -> next w;
		Some w1 -> case next w of {
			None -> rep1 p1 next w1;
			Some w2 -> Some w; 
		};
	};
	case p1 return w of {
		None -> next w;
		Some w1 -> case next w of {
			None -> rep1 p1 next w1;
			Some w2 -> rep1 p1 next w1; 
		};
	};
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