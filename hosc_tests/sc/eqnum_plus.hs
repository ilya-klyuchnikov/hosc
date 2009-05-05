data Number = Z | S Number;
data Boolean = True | False;

eqnum  (plus a b) (plus b a)

where

plus = \x2 y2 ->
  		case x2 of {
  			Z -> y2; 
  			S x1 -> S (plus x1 y2);
  		};

eqnum = \x3 y3 ->
	case x3 of {
		Z -> case y3 of {Z -> True; S y4 -> False;};
		S x5 -> case y3 of {Z -> False; S y5 -> eqnum x5 y5;};
	};