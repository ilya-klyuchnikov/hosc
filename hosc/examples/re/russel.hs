data U = MkU (U -> Bool);

russel (MkU russel)

where

russel = \u -> case u of {
	MkU p -> p u;
};