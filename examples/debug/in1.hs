{-# LANGUAGE NoImplicitPrelude#-}

data Term a = Cst a | Opr (a -> a -> a) (Term a) (Term a);

data Ctx a
 = CtxHole | CtxLeft (a -> a -> a) (Ctx a) (Term a) | CtxRight (a -> a -> a) a (Ctx a);

data Redex a = Redex (a -> a -> a) a a;
data ValDec a = Val a | Dec (Redex a) (Ctx a);


eval (Opr f (Opr g (Cst a) (Cst b)) (Opr h (Cst c) (Cst d))) CtxHole

where

contract = \r -> case r of {
  Redex f c1 c2 -> Cst(f c1 c2); };

decTerm = \t ctx -> case t of {
  Cst c -> decCtx ctx c;
  Opr f t1 t2 -> decTerm t1 (CtxLeft f ctx t2);
};

decCtx = \ctx c -> case ctx of {
  CtxHole -> Val c;
  CtxLeft f ctx0 t2 -> decTerm t2 (CtxRight f c ctx0);
  CtxRight f c1 ctx0 -> Dec (Redex f c1 c) ctx0;
};

decompose = \t -> decTerm t CtxHole;

refocus = \t ctx -> decompose (recompose ctx t);

iterate = \state -> case state of {
  Val c -> c;
  Dec r ctx ->
    iterate (decompose (recompose ctx (contract r)));
  };

normalize = \ t -> iterate (decompose t);

-- recompose :: Ctx t -> Term t -> Term t
recompose = \ctx t -> case ctx of {
  CtxHole -> t;
  CtxLeft f ctx1 t2 -> recompose ctx1 (Opr f t t2);
  CtxRight f c ctx1 -> recompose ctx1 (Opr f (Cst c) t);
};

-- eval :: Term a -> Context a -> a;
eval = \term context ->
      case term  of {
        Cst v ->
          case  context  of {
            CtxHole  -> v ;
            CtxLeft f con t -> eval t (CtxRight f v con);
            CtxRight f t con -> eval (recompose con (Cst (f t v))) CtxHole;
          };
        Opr g t1 t2 -> eval t1 (CtxLeft g context t2);
      };