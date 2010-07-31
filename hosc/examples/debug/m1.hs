data Term a = Lit a | Oper (a -> a -> a) (Term a) (Term a);

data Ctx a = Redex (a -> a -> a) a a | CL (a -> a -> a) (Ctx a) (Term a) | CR (a -> a -> a) a (Ctx a);
data Decomposition a = Val a | Dec (Ctx a);

data Wrapper a = W a; 

normalize expr

where

createContext = \f t1 t2 -> case t1 of {
	Lit x -> case t2 of {
		Lit y -> (Redex f x y);
		Oper g y z -> CR f x (createContext g y z);
	};
	Oper g x y -> CL f (createContext g x y) t2; 
};

decompose = \t -> case t of {
	Lit n -> Val n;
	Oper f t1 t2 -> Dec (createContext f t1 t2);
};

reduce = \ctx -> case ctx of {
	Redex f x y -> Lit (f x y);
	CL f ctx1 t -> Oper f (reduce ctx1) t;
	CR f v ctx1 -> Oper f (Lit v) (reduce ctx1);
};

normalize = \e -> case (decompose e) of {
	Val a -> Lit a;
	Dec ctx -> normalize (reduce ctx);
};