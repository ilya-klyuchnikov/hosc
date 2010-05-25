{-# LANGUAGE NoImplicitPrelude#-}

data Maybe a = Just a | Nothing;

x where

compose = \f g x ->  f (g x);

fix = \f -> f (fix f);

return = \x -> Just x;

fmap = \f m -> case m of {
	Nothing -> Nothing;
	Just x -> Just (f x);
};

join = \m k -> case m of {
	Nothing -> Nothing;
	Just x -> k x;
};

mfix = \f -> f (unJust (mfix f));

unJust = \m -> case m of {
	Just x -> x;
	Nothing -> bottom;
}; 

bottom = bottom;

liftM = \f m1 -> join m1 (\x1 -> return (f x1));

id = \m -> case m of {
	Nothing -> Nothing;
	Just x -> Just x;
};

gid = \x -> x;