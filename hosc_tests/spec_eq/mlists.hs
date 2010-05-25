{-# LANGUAGE NoImplicitPrelude#-}

data List a = Nil | Cons a (List a);

x where

compose = \f g x ->  f (g x);

foldr = \f z xs -> 
	case xs of {
		Nil -> z;
		Cons y ys -> f y (foldr f z ys); 
	};
	
app = \xs ys -> 
	case xs of {
		Nil -> ys; 
		Cons x1 xs1 -> Cons x1 (app xs1 ys); 
	};
	
map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> Cons (f x1) (map f xs1);
  };
  
fmap = map;
  
-- >>=
join = \m k -> foldr (compose app k) Nil m;
return = \x -> Cons x Nil;

id = \x -> case x of {
	Nil -> Nil;
	Cons y ys -> Cons y (id ys);
};

-- generic id
gid = \x -> x;

fix = \f -> f (fix f);

head = \xs -> 
	case xs of {
		Cons y ys -> y;
		Nil -> bottom;
	};
	
bottom = bottom;
	
tail = \xs ->
	case xs of {
		Nil -> Nil;
		Cons y ys -> ys;
	};

-- see 

-- mfix :: (a -> List a) -> List a
mfix = \f -> 
	case fix (\x -> f (head x)) of {
		Nil -> Nil;
		Cons y ys -> Cons y (mfix (\z -> tail (f z)));
	};
	
liftM = \f m1 -> join m1 (\x1 -> return (f x1));
--fix f = let x = f x in x;

{-
Laws:
return a >>= k  ==  k a
	join (return a) k = k a
	
m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

join m (\x -> join (k x) h) = join (join m k) h

-}   