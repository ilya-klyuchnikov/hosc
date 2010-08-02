{-# LANGUAGE NoImplicitPrelude#-}
data Bool = True | False;
data Cell l = KC (Cell l) l;

addSB c1 True where

c1 = KC choice{c2; c3;} (\f -> f True True False);
c2 = KC choice{c3; c1;} (\f -> f False True True);
c3 = KC c3 (\f -> f False False True);

a = \x y z -> x;
b = \x y z -> y;
c = \x y z -> z;

cond = \s -> or (s b) (s c);

cond1 = \s -> s b;

loop = \cell -> choice{stop cell; go cell loop;};

stop = \cell -> case cell of {KC next l -> cond l;};
go = \cell f -> case cell of {KC next l -> f next;};

or = \x y ->
  case x of {
    True -> True;
    False -> y;
};

addSB = \cell nb -> case cell of {KC next l -> KC (addSB next nb) (\f -> f l nb);};