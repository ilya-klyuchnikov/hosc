{-# LANGUAGE NoImplicitPrelude#-}

data Bool = True | False;
data State = S Bool Bool;
data List a = Cons a (List a);
data Unit = U;

--u pp qq states where

--ku1 return pp qq states  where
--u1 pp qq states where

--kx return pp states where

type Formula a = List a;
type KFormula a = List a;

x = \f1 ss -> f1 (tail ss);
kx = \k f1 ss -> (ktail (comp k f1) ss);

u = \f1 f2 -> or1 f2 (and1 f1 (x (u f1 f2)));
u1 = \f1 f2 -> (x (u f1 f2));
ku1 = \k f1 f2 -> (kx k (ku1 k f1 f2));

-- U f1 f2 = f2 OR (f1 AND (U f1 f2))

--ku2 = \k f1 f2 -> f2 (\v -> kor1 ... v )    

comp = \k h -> (\v -> k h v);

f = \f1 -> u true f1;
g = \f1  -> not1 (not1 (f (not1 f1)));
r = \f1 f2 -> not1 (u (not1 f1) (not1 f2));

or1 = \f1 f2 ss -> not (and (f1 ss) (f2 ss));
kor1 = \k f1 f2 ss -> (kand (knot k) (f1 ss) (f2 ss));
and1 = \f1 f2 ss -> and (f1 ss) (f2 ss);
kand1 = \k f1 f2 ss -> kand k (f1 ss) (f2 ss);
not1 = \f1 ss -> not (f1 ss);
knot1 = \k f1 ss -> knot k (f1 ss); 

and = \x1 y1 -> case x1 of { True -> y1; False -> False; };
kand = \k x1 y1 -> case x1 of { True -> k y1; False -> k False; };
not = \x1 -> case x1 of { True -> False; False -> True;};
knot = \k x1 -> case x1 of { True -> k False; False -> k True;};
or = \x1 y1 -> not (and x1 y1);
kor = \k x1 y1 ->  (kand (knot k) x1 y1);

true  = \ss -> True;
ktrue  = \k ss -> k True;
false = \ss -> False;
kfalse = \k ss -> k False;


s1 = \s -> case s of {S x1 x2 -> x1;};
ks1 = \k s -> case s of {S x1 x2 -> k x1;};
s2 = \s -> case s of {S x1 x2 -> x2;};
ks2 = \k s -> case s of {S x1 x2 -> k x2;};

ps1 = app s1;
kps1 = \ k -> kapp k s1;
ps2 = app s2;
kps2 = \k -> kapp k s2;


app = \f1 ls -> case ls of { Cons l1 ls1 -> f1 l1;};
kapp = \k f1 ls -> case ls of { Cons l1 ls1 -> k (f1 l1);};

head = \ls -> case ls of {Cons l1 ls1 -> l1; };
khead = \k ls -> case ls of {Cons l1 ls1 -> k l1; };
tail = \ls -> case ls of {Cons l1 ls1 -> ls1;};
ktail = \k ls -> case ls of {Cons l1 ls1 -> k ls1;};

return = \y -> y;