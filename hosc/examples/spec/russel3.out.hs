data Bool  = True | False ;
data U  = MkU (U -> Bool) Nat;
data Nat  = Z | S Nat;

letrec f = \x-> f (S x) in f Z
