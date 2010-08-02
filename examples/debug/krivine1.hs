data Nat = Z | S Nat;
data List a = Nil | Cons a (List a);
data Term = Var Nat | Lam Term | App Term Term | Ccc Term;

data Closure = CloGnd Term (List Closure) 
			| CloApp Closure Closure
			| CloCcc Closure
			| CloCtx Context;

data Context = RC0 | RC1 Context Closure | RC2 Context;

data Redex = Access Nat (List Closure)
			| Beta Closure
			| PropApp Term Term (List Closure)
			| PropCcc Term (List Closure)
			| Cwcc Closure;
			
data Option a b = None | Some a b;
			
contract r rc 

where 

lookup = \i env ->
  case env of {
    Cons n env1 ->
      case i of {
        Z -> n;
        S i1 -> lookup i1 env1;
      };
  };

-- contract :: Redex -> Context -> Option Closure Context;
contract = \red rc -> case red of {
	Access i e -> Some (lookup i e) rc;
	Beta con -> case rc of {
		RC1 rc1 c1 -> case con of {
			
		}; 
	};
}; 

traverse = \term env rc -> case term of {
	Lam t -> case rc of {
		RC1 rc1 c -> traverse t (Cons c env) rc1;
		RC0 -> Some (CloGnd term env) rc;
		RC2 rc2 -> Some (CloGnd term env) rc;
	};
	Var x -> Some (CloGnd term env) rc;
	App x y -> Some (CloGnd term env) rc;
	Ccc x -> Some (CloGnd term env) rc;
};