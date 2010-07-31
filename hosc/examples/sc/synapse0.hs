data Action = RM | WH;
data Number = Z | S Number;
data State = State Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

test (loop (State (S x) Z Z) y add)

where

test = \state -> case state of {
  State i d v -> case d of {
    S d1 -> case d1 of {
      S d2-> False; 
      Z -> case v of { 
      	S x -> False; 
      	Z -> True; 
      };
    };
    Z -> True;
  };
};

loop = \state acts add ->
    case acts of {
      Nil -> state;
      Cons a as -> loop (act state a add) as add;
    };

act = \state a add -> case state of {
	State i d v -> case a of {
		RM -> case i of {
			S x-> State (add x d) Z (S v);
		};
		WH -> case i of {
			Z -> case v of { 
				S x-> State (add (add i d) x) (S Z) Z;
			};
			S x -> case v of {
				Z -> State (add (add i d) v) (S Z) Z; 
				S y -> State (add (add i d) y) (S Z) Z;
			};
       	};
	};
};