data Nat = Z | S Nat;

ack n m where

ack = \x y ->
	case x of {
		Z -> S y;
		S x1 -> case y of {
			Z -> ack x (S Z);
			S y1 -> ack x (ack (S x) y);
		};
	};