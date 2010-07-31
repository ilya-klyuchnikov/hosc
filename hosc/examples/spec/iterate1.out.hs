data List a = Nil | Cons a (List a);
data Nat = Z | S Nat;

letrec 
	f = \w -> Cons w (f (S w)) 
in f Z