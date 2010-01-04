data Option a = Some a | None;
data Nat = Z | S Nat;

(addOpt (Some x) None)

where

-- Some Nat -> Some Nat -> Some Nat
addOpt = \xo yo -> 
  case xo of {
  	Some x -> case x of { 
  		Z -> yo;
  		S x1 -> case (addOpt (Some x1) yo) of {Some r -> Some (S r);};
  	};
  };