data Bool = True | False;
data U = MkU (U -> Bool);

(\u -> case u of {MkU p -> p (MkU p);}) 
	(MkU (\u -> case u of {MkU p -> p (MkU p);}))