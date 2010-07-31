data Bool = True | False;
data List a = Nil | Cons a (List a);
data Tree a = Leaf a | Fork (Tree a) (Tree a);

--eqBoolList Nil Nil
--eqBoolList (Cons True Nil) Nil
--eqBoolList Nil (Cons True Nil)
--eqBoolList (Cons True Nil) (Cons True Nil)
--eqBoolList (Cons True Nil) (Cons False Nil)
eqTreeLeaves

where

flatten = \t -> flattenAcc t Nil;

flattenAcc = \t u -> case t of {
  Leaf a -> Cons a u;
  Fork t1 t2 -> flattenAcc t1 (flattenAcc t2 u);
  };

not = \x -> case x of {
  True -> False;
  False -> True;
  };

eqBool = \x y -> case x of {
  True -> y;
  False -> not y;
  };

eqList = \eq u v -> case u of {
  Nil -> case v of {
    Nil -> True;
    Cons y v1 -> False;
    };
  Cons x u1 -> case v of {
    Nil -> False;
    Cons y v1 -> case eq x y of {True -> eqList eq u1 v1; False -> False;};
    };
  };

eqBoolList = eqList eqBool;

eqTreeLeaves = \t1 t2 -> eqBoolList (flatten t1) (flatten t2);