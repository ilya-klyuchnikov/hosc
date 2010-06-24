{-# LANGUAGE NoImplicitPrelude#-}

data Bool = True | False;
data State = S Bool Bool;
data List a = Cons a (List a);

u pp qq states where

--f where

x = \f1 ss -> f1 (tail ss);
u = \f1 f2 -> or1 f2 (and1 f1 (x (u f1 f2)));

--u = \f1 f2 ss -> case (f2 ss) of {
--	True -> True;
--	False -> case (f1 ss) of {
--		True -> u f1 f2 (tail ss);
--		False -> False;
--	};
--}; 
f = \f1 -> u true f1;
--f = \f1 states -> case (f1 states) of {
--	True -> True;
--	False -> f f1 (tail states);
--};
g = \f1  -> not1 (not1 (f (not1 f1)));
r = \f1 f2 -> not1 (u (not1 f1) (not1 f2));

head = \ls -> case ls of {Cons l1 ls1 -> l1; };
tail = \ls -> case ls of {Cons l1 ls1 -> ls1;};

--or1 = \f1 f2 ss -> or (f1 ss) (f2 ss);
or1 = \f1 f2 ss -> not (and (f1 ss) (f2 ss));
and1 = \f1 f2 ss -> and (f1 ss) (f2 ss);
not1 = \f1 ss -> not (f1 ss); 

and = \x1 y1 -> case x1 of { True -> y1; False -> False; };
not = \x1 -> case x1 of { True -> False; False -> True;};
or = \x1 y1 -> not (and x1 y1);

true  = \ss -> True;
false = \ss -> False;

s1 = \s -> case s of {S x1 x2 -> x1;};
s2 = \s -> case s of {S x1 x2 -> x2;};
ps1 = app s1;
ps2 = app s2;

app = \f1 ls -> case ls of { Cons l1 ls1 -> f1 l1;};