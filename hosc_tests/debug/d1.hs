data Bool = True | False;

data Term a
 = Cst a
 | Opr (a -> a -> a) (Term a) (Term a);

data Ctx a
 = CtxHole
 | CtxLeft (a -> a -> a) (Ctx a) (Term a)
 | CtxRight (a -> a -> a) a (Ctx a);

data PotRed a = PotRed (a -> a -> a) a a;

data ValDec a = Val a | Dec (PotRed a) (Ctx a);

--refocus t ctx
--decTerm t ctx
--normalize t

iterate (decTerm t CtxHole)

where

contract = \r -> case r of {
  PotRed f c1 c2 -> Cst(f c1 c2); };

decTerm = \t ctx -> case t of {
  Cst c -> decCtx ctx c;
  Opr f t1 t2 -> decTerm t1 (CtxLeft f ctx t2);
  };

decCtx = \ctx c -> case ctx of {
  CtxHole -> Val c;
  CtxLeft f ctx0 t2 -> decTerm t2 (CtxRight f c ctx0);
  CtxRight f c1 ctx0 -> Dec (PotRed f c1 c) ctx0;
  };

decompose = \t -> decTerm t CtxHole;

recompose = \ctx t -> case ctx of {
  CtxHole -> t;
  CtxLeft f ctx0 t2 -> recompose ctx0 (Opr f t t2);
  CtxRight f c ctx0 -> recompose ctx0 (Opr f (Cst c) t);
};

iterate = \state -> case state of {
  Val c -> c;
  Dec r ctx -> iterate (decTerm (contract r) ctx);
  };

normalize = \ t -> iterate (decompose t);