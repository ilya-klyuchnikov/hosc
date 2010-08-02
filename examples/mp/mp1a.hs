data Option a = Some a | None;
data Nat = Z | S Nat;
data Bool = True | False;

addOpt (Some x) None

-- oneIsZero None None
-- oneIsZero (Some x) (Some y)

where

addOpt = \xo yo -> 
  case xo of {
  	Some x -> case x of { 
  		Z -> yo;
  		S x1 -> case (addOpt (Some x1) yo) of {Some r -> Some (S r); None -> None;};
  	};
  	None -> None;
  };

multIsZero = \xo yo ->
	case xo of {
		Some x -> case x of {
			Z -> Some True; 
			S x1 -> case yo of {
				None -> None; 
				Some y -> case y of {
						Z -> Some True; 
						S y1 -> Some False;
					};
			};
		};
		None -> case yo of {
			None -> None; 
			Some y -> case y of {
						Z -> Some True; 
						S y1 -> None;
					  };
		};
	};