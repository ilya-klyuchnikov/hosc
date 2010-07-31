{-# LANGUAGE NoImplicitPrelude#-}

data Term a = Cst a | Opr (a -> a -> a) (Term a) (Term a);

data Ctx a = Empty | Left (a -> a -> a) (Ctx a) (Term a) | Right (a -> a -> a) a (Ctx a);

--eval (Opr f x y) context

eval1 t

--recompose Empty (eval (recompose con t) Empty)
--recompose con (eval t Empty)
   
where

x = x;
y = y;

-- recompose :: Ctx t -> Term t -> Term t
recompose = \ctx t -> case ctx of {
  Empty -> t;
  Left f ctx1 t2 -> recompose ctx1 (Opr f t t2);
  Right f c ctx1 -> recompose ctx1 (Opr f (Cst c) t);
};
-- eval :: Term a -> Context a -> a;
eval = \term context ->
      case term  of {
        Cst v ->
          case  context  of {
            Empty  -> v ;
            Left f con t -> eval t (Right f v con);
            Right f t con -> eval (recompose con (Cst (f t v))) Empty;
          };
        Opr g t1 t2 -> eval t1 (Left g context t2);
      };

eval1 = \ term -> case term of {
	Cst a -> a;
	Opr f t1 t2 -> f (eval1 t1) (eval1 t2);
};