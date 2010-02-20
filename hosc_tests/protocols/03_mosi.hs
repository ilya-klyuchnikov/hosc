data Number = Z | S Number;
data Action = RM | W0 | WI | WS | SE | WBM | WB0;
data State = State Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);
data Result = Result Boolean Boolean Boolean;

-- TODO: workaround (f s)
--checkAll (loop (State (S i) Z Z Z) (f s)) where
Result
	(checkState check1 (loop (State (S i) Z Z Z) (f s)))
	(checkState check1 (loop (State (S i) Z Z Z) (f s)))
	(checkState check1 (loop (State (S i) Z Z Z) (f s))) 
where
	
loop = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> loop (react state a) as;
};
react = \state action -> case state of {State invalid modified shared owned ->
	case action of {
		RM -> rm invalid modified shared owned;
		W0 -> w0 invalid modified shared owned;
		WI -> wi invalid modified shared owned;
		WS -> ws invalid modified shared owned;
		SE -> se invalid modified shared owned;
		WBM -> wbm invalid modified shared owned;
		WB0 -> wb0 invalid modified shared owned;
	};
};
rm = \invalid modified shared owned ->
	case invalid of {
		S i -> State i Z (S shared) (add modified owned);
	};
w0 = \invalid modified shared owned ->
	case owned of {
		S o -> State (add o (add shared (add modified invalid))) (S Z) Z Z;
	};
wi = \invalid modified shared owned ->
	case invalid of {
		S i -> State (add i (add shared (add modified owned))) (S Z) Z Z;
	};
ws = \invalid modified shared owned ->
	case shared of {
		S s -> State (add s (add invalid (add modified owned))) (S Z) Z Z;
	};
se = \invalid modified shared owned ->
	case shared of {
		S s -> State (S invalid) modified s owned;
	};
wbm = \invalid modified shared owned ->
	case modified of {
		S m -> State (S invalid) m shared owned;
	};
wb0 = \invalid modified shared owned ->
	case owned of {
		S o -> State (S invalid) modified shared o;
	};
checkAll = \state -> case state of {State invalid modified shared owned ->
	Result
		(check1 invalid modified shared owned)
		(check2 invalid modified shared owned)
		(check3 invalid modified shared owned);
};
check = \state -> case state of {State invalid modified shared owned ->
	check3 invalid modified shared owned;
};

checkState = \f state -> case state of {State invalid modified shared owned ->
	f invalid modified shared owned;
};
check1 = \i m s o -> case o of {S o1 -> case o1 of {S o2 -> False;};};
check2 = \i m s o -> case o of {S o1 -> case s of {S s1 -> False;};};
check3 = \i m s o -> case m of {S m1 -> case m1 of { S m2 -> False;};};
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};