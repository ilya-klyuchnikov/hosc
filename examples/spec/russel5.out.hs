data Bool  = True | False ;
data U  = MkU (U -> Bool) Nat;
data Nat  = Z | S Nat;

letrec f = \y -> f (S y) in f x
