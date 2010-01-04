data Action  = RM  | WH ;
data Number  = Z  | S Number;
data State  = State Number Number Number;
data Boolean  = True  | False ;
data List a = Nil  | Cons a (List a);

case (case (v136) of {
	Nil  -> (((act (((act (State (S x) (Z ) (Z ))) v57) add1)) v135) add1); 
	Cons v1465 v1466 -> (((loop (((act (((act (((act (State (S x) (Z ) (Z ))) v57) add1)) v135) add1)) v1465) add1)) v1466) add1);}) 
of {
	State v1467 v1468 v1469 -> 
		case (v1468) of {
		S v1470 -> case (v1470) of {S v1471 -> (False ); Z  -> case (v1469) of {S v1472 -> (False ); Z  -> (True );};}; 
		Z  -> (True );};
	}

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
