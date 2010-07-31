{-# LANGUAGE NoImplicitPrelude#-}

data KList a k = KNil | KCons a (k -> KList a k);
data List a = Nil | Cons a (List a);
data En = A | B;

--tr (kapp xs ys) where

kapp = \xs ys -> \k -> 
  case (xs (\v -> v)) of {KNil -> ys k; KCons z zs -> KCons (k z) (\l -> kapp zs ys l);};
  
capp = \xs ys k ->
	case xs of {
		Nil -> k Nil;
		Cons z zs -> k (Cons z (capp zs ys (\x -> x)));
	};