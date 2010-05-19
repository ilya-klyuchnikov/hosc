{-# LANGUAGE NoImplicitPrelude#-}

data ListR a = NilR | Cons a (ListR a);
data ListL a = NilL | Snoc (ListL a) a;
data Nat = Z | S Nat;
data Boolean = True | False;
data Pair a b = P a b;

x where

outl = \p -> case p of {
	P a b -> a;
};

outr = \p -> case p of {
	P a b -> b;
};

plus = \x y -> 
	case x of {
		Z -> y;
		S x1 -> S (plus x1 y);
	};

compose = \f g x ->  f (g x);
	
cons = \x xs -> Cons x xs;
snoc = \xs x -> Snoc xs x;

concatR = \xs ->
	case xs of { 
		NilR -> NilR;
		Cons x1 xs1 -> appendR x1 (concatR xs1);
	};

-- ++
appendR = \xs ys ->
	case xs of {
		NilR -> ys;
		Cons x1 xs1 -> Cons x1 (appendR xs1 ys);
	};

-- == map
listR = \f xs ->
	case xs of {
		NilR -> NilR;
		Cons x1 xs1 -> Cons (f x1) (listR f xs1);
	};

foldR = \c h xs -> 
	case xs of {
		NilR -> c;
		Cons y ys -> h y (foldR c h ys); 
	};
	
foldR1 = \c h xs -> 
	case xs of {
		NilR -> c;
		Cons y ys -> h (P y (foldR1 c h ys)); 
	};

-- curried version of ++
catR = foldR id (\x f y -> cons x (f y));

sum = foldR Z plus;
lengthR = \xs -> 
	case xs of {
		NilR -> Z;
		Cons y ys -> S (lengthR ys);
	};  

id = \x -> x;

-- convert L2R :: ListL a -> ListR a;
convertL2R = \xs ->
	case xs of {
		NilL -> NilR;
		Snoc ys y -> snocr (convertL2R ys) y;
	};
	
-- snocr :: ListR a -> a -> ListR a;
snocr = \ys y -> 
	case ys of {
		NilR -> Cons y NilR;
		Cons x xs -> Cons x (snocr xs y);
	};
	
wrapR = \x -> Cons x NilR;
nilpR = \x -> NilR;

cond = \p f g a -> 
	case (p a) of {
		True -> f a;
		False -> g a;
	};
	
filterR = \p xs ->
  case xs of {
    NilR -> NilR;
    Cons x xs1 ->
      case p x of {
         True -> Cons x (filterR p xs1);
         False -> filterR p xs1;
      };
  };
  
outl1 = \xs -> case xs of {
	Cons y ys -> y;
};

outr1 = \xs -> case xs of {
	Cons y ys -> ys;
};

uncurry = \f p -> case p of {
	P x y -> f x y;
};

curry = \f b c -> f (P b c);