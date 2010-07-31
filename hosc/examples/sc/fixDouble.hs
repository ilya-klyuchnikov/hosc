data Val a = Box a | Fun (Val a -> Val a);
data Nat = Z | S Nat;

unbox (unfun getZero (Box x))

where

unfun = \x -> case x of {
	Box y -> bottom;
	Fun f -> f;
};
unbox = \d -> case d of {
	Box v -> v;
	Fun f -> bottom;
};
bottom = bottom;
fix = \f -> (\x -> f (unfun x x)) (Fun (\x -> f (unfun x x)));
getZero = fix (\f -> Fun (
	\x -> case unbox x of {
		Z -> Box Z; 
		S x1 -> unfun f (Box x1);
	}
));