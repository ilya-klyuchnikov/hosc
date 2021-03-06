-- fibbonachi function

data Nat = Z | S Nat;

fib n add

where 

fib = \x f ->
	case x of {
		Z -> 
			S Z;
		S x1 -> 
			case x1 of {
				Z -> S Z;
				S x2 -> f (fib x1 f) (fib x2 f);
			}; 
	};