{-# LANGUAGE NoImplicitPrelude#-}

n0 = \x y -> y;
n1 = \x y -> x y;
n2 = \x y -> x (x y);
n3 = \x y -> x (x (x y)); 

true  = \a b -> a;
false = \a b -> b;

not = \m a b   -> m b a;
xor = \m n a b -> m (n b a) (n a b);

pair = \x y z -> z x y;
fst  = \p -> p (\x y -> x);
snd  = \p -> p (\x y -> y);

fix = \f -> f (fix f);

even = \n -> n not true;
odd = \n -> n not false;

add  = \m n -> (\f x -> m f (n f x));
pred = \n f x -> n (\g h -> h (g f)) (\u -> x) (\v -> v);
sub  = \m n -> (n pred) m;
mult = \m n f -> n (m f);

comp = \f g x -> (f g) x;

ifThenElse = \f g x -> f g x;

or  = \a b -> ifThenElse a true b;
and = \a b -> ifThenElse a b false;

and1 = \w y -> w y false;
or1  = \p v -> p true v;