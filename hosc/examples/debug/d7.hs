data Nat = Z | S Nat;

ack (S (S Z)) where

ack = \m n -> case x of {
	Z -> S n;
	S m1 -> case n of {
		Z -> ack m1 (S Z);
		S n1 -> ack m1 (ack m n1);
	};
};