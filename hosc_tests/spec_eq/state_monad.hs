{-# LANGUAGE NoImplicitPrelude#-}
data Pair a b = P a b;
--type StateTr s a = s -> P a s;
x where

compose = \f g x ->  f (g x);
outl = \p -> case p of {P a b -> a;};
outr = \p -> case p of {P a b -> b;};
gid = \x -> x;
idP = \p -> case p of {P a b -> P a b;};
fmap = \f m s -> case m s of {P a s1 -> P (f a) s1;};
id = \f x -> idP (f x);
return = \a s -> P a s;
join = \m k s -> case m s of {P a s1 -> k a s1;}; 
bind = \m k -> join m (\x -> k);
mzero = \s -> undefined;
undefined = undefined;