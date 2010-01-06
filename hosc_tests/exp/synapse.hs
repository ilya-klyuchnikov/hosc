data Action = RM | WH;
data Number = Z | S Number;
data State = State Number Number Number | Stop;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

test (loop (State (S x) Z Z) add)

where

test = \state -> case state of {
  State i d v -> case d of {
    S d1 -> case d1 of {
      S d2-> False; 
      Z -> case v of { S x -> False; Z -> True; };
    };
    Z -> True;
  };
  Stop -> True;
};

loop = \state add ->
	[state | loop (act state add) add]; --non-det

act = \state f -> case state of {
	Stop -> Stop;
	State i d v -> [actRM i d v f | actWH i d v f]; -- non-det!!
};


actRM = \i d v f -> 
	case i of {
		Z -> Stop; 
		S x -> State (f x d) Z (S v);
	};

actWH = \i d v f -> 
	case i of {
		Z -> case v of {
			Z -> Stop; 
			S x-> State (f (f i d) x) (S Z) Z;
		};
		S x -> case v of {
			Z -> State (f (f i d) v) (S Z) Z; 
			S y -> State (f (f i d) y) (S Z) Z;
		};
	};