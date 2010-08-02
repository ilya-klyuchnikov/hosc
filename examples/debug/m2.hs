data Term a = Lit a | Oper (a -> a -> a) (Term a) (Term a);
data Val a = Val a;

eval t where

eval = \t -> case t of {
	Lit a -> Lit a;
	Oper f x y -> case (eval x) of {
		Lit x1 -> case (eval y) of {
			Lit y1 -> Lit (f x1 y1);
		};
	};
};
