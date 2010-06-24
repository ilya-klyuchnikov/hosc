{-# LANGUAGE NoImplicitPrelude#-}
data Bool = True | False;
--data CName = S1 | S2 | S3;
data Atom = A | B | C;
data List a = Nil | Cons a (List a);
--data KModel = Model (List CName) (List (CName -> (List CName))) (List);
data KCell = KC (List KCell) (Atom -> Bool);

c1 where

c1 = KC (Cons c2 (Cons c3 Nil)) f1;
f1 =\a -> case a of {A -> True; B -> True; C -> False;};

c2 = KC (Cons c1 (Cons c3 Nil)) f2;
f2 =\a -> case a of {A -> False; B -> True; C -> True;};

c3 = KC (Cons c3 Nil) f3;
f3 =\a -> case a of {A -> False; B -> False; C -> True;};   