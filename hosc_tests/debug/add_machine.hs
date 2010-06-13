data Term a = Lit a | Add (Term a) (Term a);

data Red a = Sum a a;
data Context a = Hole (Red a) | C1 (Context a) (Term a) | C2 a (Context a);
data Dec a = Val a | Ctx (Context a);

-- interesting: sc without control - seems we need simplify there:
{-
==========HE
case  (decompose t)  of { Ctx $9 -> ((normalize2 f) ((step f) $9)); Val $8 -> $8; }
case 
case  (decompose $19)  of {
  Ctx $29 -> (Ctx (C1 $29 $20));
  Val $30 -> case  (decompose $20)  of { Ctx $32 -> (Ctx (C2 $30 $32)); Val $31 -> (Ctx (Hole (Sum $30 $31))); };
}
 of {
  Ctx $34 -> ((normalize2 f) ((step f) $34));
  Val $33 -> $33;
}
-}

normalize2 f t 

{-
normalize f t 

with/without control and reverse generalizing (generalizing down node) seems non-terminating!
-}

where

--contract :: (a -> a -> a) -> Red a -> a;
contract = \f red -> case red of {
	Sum x y -> f x y;
};

--decompose :: Term a -> Dec a;
decompose = \term -> case term of {
	Lit n -> Val n;
	Add t1 t2 -> case (decompose t1) of {
		Ctx ctl -> Ctx (C1 ctl t2);
		Val x -> case (decompose t2) of {
			Val y -> Ctx (Hole (Sum x y));
			Ctx ctr -> Ctx (C2 x ctr);
		};
	};
};

--redex :: Context a -> Red a;
redex = \ctx -> case ctx of {
	Hole r -> r;
	C1 ctl t -> redex ctl;
	C2 a ctr -> redex ctr;
};

--plug :: a -> Context a -> Term a;
plug = \val ctx -> case ctx of {
	Hole r -> Lit val;
	C1 ctl t -> Add (plug val ctl) t;
	C2 a ctr -> Add (Lit a) (plug val ctr);
};

--reduce :: (a -> a -> a) -> Term a -> Term a;
reduce = \f term -> case (decompose term) of {
	Val x -> Lit x;
	Ctx ctx -> plug (contract f (redex ctx)) ctx;
};

normalize = \f term -> case (reduce f term) of {
	Lit x -> Lit x;
	Add x y -> normalize f (Add x y);
};

normalize1 = \f term -> case (reduce f term) of {
	Lit x -> Lit x;
	Add x y -> normalize f (Add (normalize1 f x) (normalize1 f y));
};


step = \f ctx -> case ctx of {
	Hole red -> contract f red;
	C1 ctl t -> Add (step f ctl) t;
	C2 a ctr -> Add (Lit a) (step f ctr); 
};

normalize2 = \f term -> case (decompose term) of {
	Val x -> x;
	Ctx ctx -> normalize2 f (step f ctx);
};