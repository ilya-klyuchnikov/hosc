import Debug.Trace;

data Op = ADD | SUB deriving (Show, Eq);

data Term = Lit Int | Oper Op Term Term deriving (Show, Eq);
data Redex = Redex Op Int Int deriving (Show, Eq);

data Ctx = CtxE | CtxL Op Ctx Term | CtxR Op Int Ctx deriving (Show, Eq);
data Dec = Val Int | Dec Redex Ctx deriving (Show, Eq);

contract :: Redex -> Term;
contract (Redex ADD x y) = Lit (x + y);
contract (Redex SUB x y) = Lit (x - y);

decomposeTerm :: Term -> Ctx -> Dec;
decomposeTerm (Lit x) con = decomposeContext con x;
decomposeTerm (Oper f t1 t2) con = decomposeTerm t1 (CtxL f con t2);

decomposeContext :: Ctx -> Int -> Dec;
decomposeContext CtxE v = Val v;
decomposeContext (CtxL f c t2) v1 = decomposeTerm t2 (CtxR f v1 c);
decomposeContext (CtxR f v1 c) v2 = Dec (Redex f v1 v2) c;

decompose :: Term -> Dec;
decompose t = decomposeTerm t CtxE;

e1 = Oper ADD (Lit 1) (Lit 2);
e2 = Oper ADD e1 (Lit 10);
e3 = Oper ADD (Lit 4) (Oper ADD (Oper SUB (Lit 0) (Lit 6)) (Lit 10));

-- e3:
-- Oper ADD (Lit 4) (Oper ADD (Oper ADD (Lit 1) (Lit 2)) (Lit 10))

--4 + ((+ 1 2) + 10)

e4 = Oper ADD (Lit 2) (Oper ADD (Lit 3) (Lit 4))

recompose :: Ctx -> Term -> Term
recompose CtxE t = t;
recompose (CtxL f c t2) t1 = recompose c (Oper f t1 t2);
recompose (CtxR f v1 c) t2 =  recompose c (Oper f (Lit v1) t2);

recomposeT c t = trace ("\trecompose " ++ (show c) ++ " " ++ (show t) ++ " => " ++ (show result)) result
	where
		result = recompose c t
decomposeT t = trace ("\tdecompose " ++ (show t) ++ " => " ++ (show result)) result
	where
		result = decompose t

iterate1 x = traceShow x (iterate0 x);

iterate0 :: Dec -> Int
iterate0 (Val x) = x;
iterate0 (Dec red c) = iterate1 (decomposeT (recomposeT c (contract red)));

normalize0 t = iterate1 (decompose t);