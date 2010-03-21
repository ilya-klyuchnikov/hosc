data D = F (D -> D);

\f -> (\y -> case y of { F g -> g; })
	(F (\x -> f ((\y -> case y of { F g -> g; }) x x)))
		(F (\x -> f ((\y -> case y of { F g -> g; }) x x)))