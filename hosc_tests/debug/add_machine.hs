data Term a = Lit a | Add (Term a) (Term a);

data Red a = Sum a a;
data Context a = Hole (Red a) | C1 (Context a) (Term a) | C2 a (Context a);
data Dec a = Val a | Ctx (Context a);

normalize2 f t where

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