{-# LANGUAGE NoImplicitPrelude#-}

data Nat = Z | S Nat;
data Tree a = Leaf a | Fork (Tree a) (Tree a);

--doFR (rfLeaf a) f

--where

pair = \x y c -> c x y;
fst = \p -> p (\x y -> x);
snd = \p -> p (\x y -> y);

leaf = \x -> Leaf x;
fork = \u v -> Fork u v;

treeFold = \t ->
  case t of {
    Leaf x -> (\f -> x);
    Fork u v -> (\f -> f (treeFold u f) (treeFold v f));
  };

treeReplace = \t ->
  case t of {
    Leaf x -> (\i -> Leaf i);
    Fork u v -> (\i -> Fork (treeReplace u i) (treeReplace v i));
  };

replFold = \t f -> treeReplace t (treeFold t f);

treeReplFold = \t ->
  case t of {
    Leaf x -> pair
      (\f -> x)
      (\i -> Leaf i);
    Fork u v -> pair
      (\f -> f (treeFold u f) (treeFold v f))
      (\i -> Fork (treeReplace u i) (treeReplace v i));
  };

doReplFold = \t f -> treeReplFold t (\doFold doRepl -> doRepl (doFold f));

rfLeaf = \x -> pair
  (\f -> x)
  (\i -> Leaf i);

rfFork = \u v -> pair
  (\f -> f (fst u f) (fst v f))
  (\i -> Fork (snd u i) (snd v i));

doFR = \t f -> (\do1 -> snd do1 (fst do1 f)) t;