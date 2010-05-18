data Bool = True | False;
data U = MkU (U -> Bool);
data Nat = Z | S Nat;

russel (MkU russel) where

russel = \u -> case u of {MkU p -> p (MkU p);};