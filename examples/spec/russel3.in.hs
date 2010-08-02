data Bool = True | False;
data U = MkU (U -> Bool) Nat;
data Nat = Z | S Nat;

russel (MkU russel Z) where

russel = \u -> case u of {MkU p n -> p (MkU p (S n));};