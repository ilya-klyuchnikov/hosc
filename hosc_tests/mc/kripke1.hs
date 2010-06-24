{-# LANGUAGE NoImplicitPrelude#-}

-- Fig. 4.1 from Karpov

data Bool = T | F;
--              p    q    r    next
data State = St Bool Bool Bool State;
data S1 = S1 Bool S1;

ss1 where

s0 = St T T F choice{s1; s2;};
s1 = St F T T s2;
s2 = St F F T choice{s2; s3;};
s3 = St F F F s1;

ss1 = S1 T ss1;

p =\s -> case s of {St x y z n -> x;};
q =\s -> case s of {St x y z n -> y;};
r =\s -> case s of {St x y z n -> z;};

or = \f1 f2 s -> case (f1 s) of { T -> T; F -> f2 s;};
and = \f1 f2 s -> case (f1 s) of { T -> f2 s;F -> F;};
not = \f1 s -> case (f1 s) of { T -> F;F -> T;};

next = \f1 s -> case s of {St x y z n -> f1 n;};
until = \f1 f2 s -> case s of {St x y z n -> case (f2 s) of {
	T -> T;
	F -> case (f1 n) of {
		T -> (until f1 f2 n);
		F -> F;
	};
};};
