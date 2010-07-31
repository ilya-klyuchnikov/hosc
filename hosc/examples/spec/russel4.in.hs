data Bool = True | False;
data U = MkU (U -> Bool) Nat;
data Nat = Z | S Nat;

(\u -> case u of {MkU p n -> p (MkU p (S n));}) 
(MkU (\u -> case u of {MkU p n -> p (MkU p (S n));}) Z)