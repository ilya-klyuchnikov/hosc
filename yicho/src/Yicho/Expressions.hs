{-#OPTIONS -fglasgow-exts -fth #-}
-- This module provides the functions to manipulate the expressions of type
-- Exp.
module Yicho.Expressions where

import Data.Maybe (fromMaybe,isJust,maybeToList)
import Language.Haskell.TH.Syntax
import Data.List(intersect,union)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib
import Debug.Trace

-- * Patterns

-- | Return free variables in 'Pat'
pat2names                  :: Pat -> [Name]
pat2names (LitP _)         = []
pat2names (VarP n)         = [n]
pat2names (TupP ps)        = concatMap pat2names ps
pat2names (ConP n ps)      = concatMap pat2names ps
pat2names (InfixP p1 n p2) = pat2names p1 ++ pat2names p2
pat2names (TildeP p)       = pat2names p
pat2names (AsP n p)        = [n] ++ pat2names p
pat2names WildP            = []
pat2names (RecP _ _)       = error "not implemented in Expressions.pat2names"
pat2names (ListP ps)       = concatMap pat2names ps
pat2names (SigP p _)       = pat2names p

----------------
-- ** Eta reduction

lamexpmatch :: Pat -> Exp -> Maybe [(Name,Exp)]
lamexpmatch (VarP x) e = Just [(x,e)]
lamexpmatch (TupP xs) (TupE es)
   = if length xs /= length es then Nothing
     else foldr accum (Just []) $ zipWith lamexpmatch xs es
  where accum (Just newmatches) (Just matches) = Just (newmatches++matches)
        accum _ _ = Nothing
lamexpmatch _ _ = Nothing

-- | Is eta redex ?
etaredex (LamE ps b) =
  case b of
    AppE f exp ->
        case lamexpmatch (last ps) exp of
          Just matches -> (and $ map check $ matches) &&
                          null (map fst matches `intersect` unboundlocals f)
          Nothing -> False
    InfixE (Just e1) op (Just exp) ->
        case lamexpmatch (last ps) exp of
          Just matches -> (and $ (map check) $ matches) &&
                          null (map fst matches `intersect`
                                (unboundlocals op `union` unboundlocals e1))
          Nothing -> False
    InfixE (Just exp) op Nothing ->
             case lamexpmatch (last ps) exp of
                  Just matches ->
                      (and $ (map check) $ matches) &&
                      null (map fst matches `intersect` unboundlocals op)
                  Nothing -> False
    e -> False
 where check (x,VarE y) = x==y
       check (_,_) = False

-- | Eta reduction
-- etaRed e | trace (show (e,if isLambda e then etaredex e else False)) False = undefined
etaRed e@(LamE ps (AppE f exp)) =
    if etaredex e then etaRed (simplifyLam (LamE (init ps) f)) else e
etaRed e@(LamE ps (InfixE m1 op (Just e2))) =
    if etaredex e
       then etaRed (simplifyLam (LamE (init ps) (InfixE m1 op Nothing))) else e
etaRed e@(LamE ps (InfixE (Just e1) op Nothing)) =
    if etaredex e
       then etaRed (simplifyLam (LamE (init ps) (InfixE Nothing op Nothing))) else e
etaRed e = e

-- | Converting `Pat' to `Exp'
pat2exp :: Pat -> Exp
pat2exp p = case p of
 LitP x    -> LitE x
 VarP x    -> VarE x
 TupP ps   -> TupE (map pat2exp ps)
 ConP s ps -> foldl1 AppE (ConE s : map pat2exp ps)
 InfixP p1 n p2 -> foldl1 AppE [name2exp n,pat2exp p1,pat2exp p2]
     where name2exp n | isGlobalN n = ConE n
                      | otherwise  = VarE n
 TildeP p  -> pat2exp p
 AsP s p   -> pat2exp p                 -- s is ignored
 WildP     -> VarE (mkName "_")         -- don't care
 RecP s fs -> RecConE s (map patfield2exp fs)
 ListP ps  -> ListE (map pat2exp ps)

exp2pat :: Exp -> Pat
exp2pat e = case e of
 LitE x    -> LitP x
 VarE x    -> VarP x
 TupE es   -> TupP (map exp2pat es)
 ListE es  -> ListP (map exp2pat es)
 _ -> error "unimplemented in exp2pat"

patfield2exp :: FieldPat -> FieldExp
patfield2exp (s,p) = (s,pat2exp p)


-- * Closures
-- | Closures of Expressions
data CL = CL { expC::Exp,       -- ^ expression
               boundsC::[Name], -- ^ names of bound variables
               frozensC::[Name] -- ^ names of frozen variables
             } deriving Show

flexC :: CL -> Bool
flexC (CL e bs fs) = flex e (bs++fs)

rigidC :: CL -> Bool
rigidC (CL {expC=VarE u,boundsC=bs,frozensC=fs}) =
    u `elem` bs || u `elem` fs || isJust (nameModule u)

isFreeC :: CL -> Bool
isFreeC cl@(CL {expC=VarE u,frozensC=fs}) = not (rigidC cl)

unboundlocalsC :: CL -> [Name]
unboundlocalsC (CL {expC=e,boundsC=bs,frozensC=fs}) =
    filter (\n-> n `notElem` fs && not (isGlobalN n)) (freevars e)

freevarsC :: CL -> [Name]
freevarsC (CL {expC=e,boundsC=bs,frozensC=fs}) =
    filter (\n-> n `notElem` bs && n `notElem` fs && not (isGlobalN n)) (freevars e)

-- * Expressions

-- | Whether the given expression is flexible
flex :: Exp -> [Name] -> Bool
flex e ns = case e of
    VarE v         -> v `notElem` ns && not (isGlobalN v)
    ConE v         -> False
    LitE _         -> False
    AppE e1 _      -> flex e1 ns
    InfixE m1 e m2 -> flex e ns
    LamE _ _       -> True
    TupE _         -> False
    CondE _ _ _    -> False
    LetE _ e       -> flex e ns
    CaseE _ _      -> False
    DoE _          -> False
    CompE _        -> False
    ArithSeqE _    -> False
    ListE _        -> False
    SigE e t       -> flex e ns
    RecConE v _    -> False
    RecUpdE e _    -> flex e ns

-- | Split an expression into the head and its arguments
headargs :: Exp -> (Exp,[Exp])
headargs (AppE f e) = let (h,as) = headargs f
                       in (h,as ++ [e])
headargs e = (e,[])

subexps :: Exp -> [Exp]
subexps e = case e of
    VarE v         -> [e]
    ConE v         -> [e]
    LitE _         -> [e]
    AppE e1 e2     -> e : subexps e1 ++ subexps e2
    InfixE m1 op m2 -> e : maybe [] subexps m1 ++ subexps op ++ maybe [] subexps m2
                       ++ case (m1,m2) of
                            (Just a,Just b) -> [InfixE m1 op Nothing]
                            _ -> []
    LamE ps b      -> e : subexps b
    TupE es        -> e : concatMap subexps es
    CondE e1 e2 e3 -> e : subexps e1 ++ subexps e2 ++ subexps e3
    LetE ds b      -> error "not implemented in Expressions.subexps"
    CaseE _ _      -> error "not implemented in Expressions.subexps"
    DoE _          -> error "not implemented in Expressions.subexps"
    CompE _        -> error "not implemented in Expressions.subexps"
    ArithSeqE _    -> error "not implemented in Expressions.subexps"
    ListE es       -> e : concatMap subexps es -- fix!
    SigE b t       -> e : subexps b
    RecConE v _    -> error "not implemented in Expressions.subexps"
    RecUpdE e _    -> error "not implemented in Expressions.subexps"

eqBox :: (Exp,[Name]) -> (Exp,[Name]) -> [Name] -> Bool
eqBox (e1,ns1) (e2,ns2) cs = f e1 e2
  where
  f e1 e2 = case (e1,e2) of
    (VarE u,VarE v) | u `elem` cs || v `elem` cs -> u==v
    (VarE u,VarE v) | u `elem` ns1 || v `elem` ns2 -> True
    (VarE u,VarE v) | u == v -> True
    (ConE u,ConE v) -> nameBase u == nameBase v &&
                       nameModule u == nameModule v &&
                       isJust (nameModule u) &&
                       isJust (nameModule v)
    (LitE u,LitE v) -> u == v
    (AppE f1 f2,AppE g1 g2) -> f f1 g1 && f f2 g2
    (InfixE _ _ _,_) -> error "not implemented in Expressions.eqBox"
    (LamE _ _,LamE _ _) -> error "not implemented in Expressions.eqBox"
    (TupE _,TupE _) -> error "not implemented in Expressions.eqBox"
    (CondE _ _ _,CondE _ _ _) -> error "not implemented in Expressions.eqBox"
    (_,LetE _ _) -> error "not implemented in Expressions.eqBox"
    (LetE _ _,_) -> error "not implemented in Expressions.eqBox"
    (CaseE _ _,_) -> error "not implemented in Expressions.eqBox"
    (_,CaseE _ _) -> error "not implemented in Expressions.eqBox"
    (DoE _,_) -> error "not implemented in Expressions.eqBox"
    (_,DoE _) -> error "not implemented in Expressions.eqBox"
    (CompE _,_) -> error "not implemented in Expressions.eqBox"
    (_,CompE _) -> error "not implemented in Expressions.eqBox"
    (ArithSeqE _,_) -> error "not implemented in Expressions.eqBox"
    (_,ArithSeqE _) -> error "not implemented in Expressions.eqBox"
    (ListE ps, ListE ts) -> and (zipWith f ps ts)
    (SigE p t, b) -> f p b
    (p, SigE t _) -> f p t
    (RecConE _ _,_) -> error "not implemented in Expressions.eqBox"
    (_,RecConE _ _) -> error "not implemented in Expressions.eqBox"
    (RecUpdE _ _,_) -> error "not implemented in Expressions.eqBox"
    (_,RecUpdE _ _) -> error "not implemented in Expressions.eqBox"
    (_,_) -> False

-- | we regard types of constans as small as possible.
--   for example,  [| foldr (+) [] |] is second-order.
secondOrderArg :: Exp -> Bool              -- fix Pat
secondOrderArg e = f (outmostlocalvars e) (lambdaBody e)
  where
  f bs e = case e of
    VarE u -> True
    ConE u -> True
    LitE u -> True
    AppE (VarE v) f2 -> not (v `elem` bs) && f bs f2
    AppE t1 t2 -> f bs t1 && f bs t2
    InfixE (Just t1) op (Just t2) -> f bs t1 && f bs op && f bs t2
    InfixE Nothing op (Just t2) -> f bs op && f bs t2
    InfixE (Just t1) op Nothing -> f bs t1 && f bs op
    InfixE Nothing op Nothing -> f bs op
    LamE ps b -> f bs b
    TupE es -> all (f bs) es
    CondE e1 e2 e3 -> f bs e1 && f bs e2 && f bs e3
    LetE _ _ -> error "not implemented in Expressions.secondOrder"
    CaseE _ _ -> error "not implemented in Expressions.secondOrder"
    DoE _ -> error "not implemented in Expressions.secondOrder"
    CompE _ -> error "not implemented in Expressions.secondOrder"
    ArithSeqE _ -> error "not implemented in Expressions.secondOrder"
    ListE es -> all (f bs) es
    SigE t _ -> f bs t
    RecConE _ _ -> error "not implemented in Expressions.secondOrder"
    RecUpdE _ _ -> error "not implemented in Expressions.secondOrder"

-- | A pattern is flat if no higher-order free variables of the pattern
--   has free higher-order variables again in its arguments.
flat :: [Name] -> Exp -> Bool
flat bs e = f bs e
  where
  f bs e = case e of
    VarE u -> True
    ConE u -> True
    LitE u -> True
    AppE t1 t2 | flex t1 bs -> f bs t1 && fa bs t2
    AppE t1 t2 -> f bs t1 && f bs t2
    InfixE _ op _ -> error "not implemented in Expressions.flat"
    LamE ps b -> f (concatMap pat2names ps++bs) b
    TupE es -> all (f bs) es
    CondE e1 e2 e3 -> f bs e1 && f bs e2 && f bs e3
    LetE ds e -> let bs' = bs ++ concatMap decname ds
                  in f bs' e            -- fix me
    CaseE e ms -> f bs e                -- fix me
    DoE _ -> error "not implemented in Expressions.flat"
    CompE _ -> error "not implemented in Expressions.flat"
    ArithSeqE _ -> error "not implemented in Expressions.flat"
    ListE es -> all (f bs) es
    SigE t _ -> f bs t
    RecConE _ _ -> error "not implemented in Expressions.flat"
    RecUpdE _ _ -> error "not implemented in Expressions.flat"
  -- | first-order arguments
  fa bs e = case e of
    VarE u -> True
    ConE u -> True
    LitE u -> True
    AppE t1 t2 | flex t1 bs -> False
    AppE t1 t2 -> fa bs t1 && fa bs t2
    InfixE _ op _ -> error "not implemented in Expressions.flat"
    LamE ps b -> fa (concatMap pat2names ps++bs) b
    TupE es -> all (fa bs) es
    CondE e1 e2 e3 -> fa bs e1 && fa bs e2 && fa bs e3
    LetE _ _ -> error "not implemented in Expressions.flat"
    CaseE _ _ -> error "not implemented in Expressions.flat"
    DoE _ -> error "not implemented in Expressions.flat"
    CompE _ -> error "not implemented in Expressions.flat"
    ArithSeqE _ -> error "not implemented in Expressions.flat"
    ListE es -> all (fa bs) es
    SigE t _ -> fa bs t
    RecConE _ _ -> error "not implemented in Expressions.flat"
    RecUpdE _ _ -> error "not implemented in Expressions.flat"

filter_wild = filter (\n -> nameBase n /= "_")

-- | The domain should be flat, scond pattern
linear :: [Name] -> Exp -> Bool
linear bs e = let xxx = noDup (filter (`notElem` bs) (filter_wild (unboundlocals e))) && args bs e
              in if xxx then xxx else trace (show (e)) xxx
  where
  noDup []    = True
  noDup (a:x) = a `notElem` x && noDup x

  args obs e = case e of
    VarE u -> True
    ConE u -> True
    LitE u -> True
    AppE t1 t2 | flex t1 bs -> args obs t1 && args' obs (lambdaBody t2)
    AppE t1 t2 {- rigid -} -> args obs t1 && args obs t2
    LamE ps b -> args' (concatMap pat2names ps ++ obs) b
    TupE es -> and (map (args obs) es)
    CondE e1 e2 e3 -> args obs e1 && args obs e2 && args obs e3
    LetE _ _ -> error "not implemented in Expressions.allvars"
    CaseE _ _ -> error "not implemented in Expressions.allvars"
    DoE _ -> error "not implemented in Expressions.allvars"
    CompE _ -> error "not implemented in Expressions.allvars"
    ArithSeqE _ -> error "not implemented in Expressions.allvars"
    ListE es -> and (map (args obs) es)
    SigE t _ -> args obs t
    RecConE _ _ -> error "not implemented in Expressions.allvars"
    RecUpdE _ _ -> error "not implemented in Expressions.allvars"
  args' fs e = noDup (filter_wild (filter (`notElem` fs) (unboundlocals e)))

-- | get rid of LamE []
simplifyLam                       :: Exp -> Exp
simplifyLam (LamE [] e)           = simplifyLam e
simplifyLam (LamE ps (LamE qs e)) = simplifyLam (LamE (ps++qs) e)
simplifyLam e                     = e

outmostlocalvars :: Exp -> [Name]
outmostlocalvars e = case simplifyLam e of
                                  LamE ps _ -> concatMap pat2names ps
                                  _         -> []

getBody :: Exp -> Exp
getBody (LamE ps b) = b
getBody e           = e

splitTup :: Exp -> ([Exp], [Name] -> Pat)
splitTup (TupE es) = (es,TupP . map VarP)
splitTup e         = ([e],\ [a] -> VarP a)

isLambda :: Exp -> Bool
isLambda (LamE (p:ps) e) = True
isLambda _               = False

lambdaBody :: Exp -> Exp
lambdaBody (LamE _ e) = lambdaBody e
lambdaBody e = e

-- | check whether outmost formal parameters are appeared in the body of the lambda abstraction
appearsin :: Exp -> Bool
appearsin e = and [ v `elem` unboundlocals (lambdaBody e) | v <- outmostlocalvars e ]

dec2exp :: Dec -> Exp
dec2exp (FunD f [cl]) = LamE [VarP f] (clause2exp cl)
-- dec2exp (FunD f cls) = LetE [FunD f cls] (VarE f) -- fix me
dec2exp (ValD pat body ds@[]) = LamE [pat] (body2exp body)

clause2exp :: Clause -> Exp
clause2exp (Clause ps b []) = LamE ps (body2exp b)
clause2exp (Clause ps b ds) = LamE ps (LetE ds (body2exp b))

body2exp :: Body -> Exp
body2exp (NormalB e) = e

match2exp :: Match -> Exp
match2exp (Match pat body ds@[]) = LamE [pat] (body2exp body)

-- * Names
isGlobalN, isLocal :: Name -> Bool
isGlobalN x = isJust (nameModule x)
isLocal x = not (isGlobalN x)

isBoundLocal, isUnboundLocal :: Name -> [Name] -> Bool
isBoundLocal x bs = isLocal x && x `elem` bs
isUnboundLocal x bs = isLocal x && x `notElem` bs

isOperator :: Exp -> Bool
isOperator (VarE x) = and [ n `elem` ".:!#$%&*+/<=>?@^|-\\" | n <- nameBase x ]
isOperator (ConE x) = and [ n `elem` ".:!#$%&*+/<=>?@^|-\\" | n <- nameBase x ]
isOperator _ = False

mkOperator :: Exp -> Exp
mkOperator exp | isOperator exp = InfixE Nothing exp Nothing
mkOperator exp = exp

unboundlocals :: Exp -> [Name]
unboundlocals e = unboundlocals' e [] []
 where
 unboundlocals' b bounds xs = case b of
   VarE x -> if x `elem` bounds || isGlobalN x then xs else x:xs
   ConE _ -> xs
   LitE _ -> xs
   AppE e1 e2 -> unboundlocals' e1 bounds (unboundlocals' e2 bounds xs)
   InfixE m1 e (Just e2) ->
       unboundlocals' e2 bounds (unboundlocals' (InfixE m1 e Nothing) bounds xs)
   InfixE (Just e1) e Nothing ->
       unboundlocals' e1 bounds (unboundlocals' e bounds xs)
   InfixE Nothing e Nothing -> unboundlocals' e bounds xs
   LamE p e -> unboundlocals' e (concatMap pat2names p++bounds) xs
   TupE es -> foldr (\e ys -> unboundlocals' e bounds ys) xs es
   CondE e1 e2 e3 -> foldr (\e ys -> unboundlocals' e bounds ys) xs [e1,e2,e3]
   LetE ds b -> unboundlocals' b (concatMap decname ds ++ bounds) xs -- Fix me!
   CaseE e ms -> unboundlocals' e bounds $
                 foldr (\e ys -> unboundlocalsMatch e bounds ys) xs ms
   DoE stms -> foldr (\e ys -> unboundlocalsStm e bounds ys) xs stms
   CompE stms -> foldr (\e ys -> unboundlocalsStm e bounds ys) xs stms
   ArithSeqE rg -> unboundlocalsRange rg bounds xs
   ListE es -> foldr (\e ys -> unboundlocals' e bounds ys) xs es
   SigE e t -> unboundlocals' e bounds xs
   RecConE _ fs -> foldr (\e ys -> unboundlocalsField e bounds ys) xs fs
   RecUpdE e fs -> unboundlocals' e bounds $
                   foldr (\e ys -> unboundlocalsField e bounds ys) xs fs

 unboundlocalsField :: FieldExp -> [Name] -> [Name] -> [Name]
 unboundlocalsField (_,e) bounds ys = unboundlocals' e bounds ys

 unboundlocalsMatch :: Match -> [Name] -> [Name] -> [Name]
 unboundlocalsMatch (Match p b ds@[]) bounds xs =
     let bounds' = bounds ++ pat2names p
      in unboundlocalsBody b bounds' (unboundlocalsDecs ds bounds' xs)

 unboundlocalsDecs :: [Dec] -> [Name] -> [Name] -> [Name]
 unboundlocalsDecs ds bounds xs =
     let bounds' = bounds ++ concatMap decname ds
      in foldr (\e ys -> unboundlocalsDec e bounds' ys) xs ds

 unboundlocalsDec :: Dec -> [Name] -> [Name] -> [Name]
 unboundlocalsDec d bounds xs = case d of
  FunD s cls -> foldr (\e ys -> unboundlocalsClause e (s:bounds) ys) xs cls
--   ValD p b ds -> let bounds' = patvars p ++ bounds ++ decnamess ds
--                   in unboundlocalsBody b bounds' $
--                      unboundlocalsDecs ds bounds' xs
  DataD _ _ _ _ _    -> []
  NewtypeD _ _ _ _ _ -> []
  TySynD _ _ _       -> []
  ClassD _ _ _ _ _   -> []
  InstanceD _ _ _    -> []
  SigD _ _           -> []
  ForeignD _         -> []

 unboundlocalsBody :: Body -> [Name] -> [Name] -> [Name]
 unboundlocalsBody b bounds xs = case b of
--    GuardedB pes ->
--        foldr (\e ys -> unboundlocals' e bounds ys) xs (uncurry (++) (unzip pes))
   NormalB e -> unboundlocals' e bounds xs

 unboundlocalsClause (Clause ps b ds) bounds xs =
     let bounds' = concatMap pat2names ps
      in unboundlocalsBody b bounds' $
         foldr (\e ys -> unboundlocalsDec e bounds' ys) xs ds

 unboundlocalsStm x bounds xs = case x of
      BindS p e -> unboundlocals' e (pat2names p++bounds) xs
--       LetS ds   -> unboundlocalsDecs ds bounds xs
      NoBindS e -> unboundlocals' e bounds xs
      ParS stmss ->
          foldr (\e ys -> unboundlocalsStm e bounds ys) xs (concat stmss)

 unboundlocalsRange rg bounds xs =
       case rg of
            FromR e -> unboundlocals' e bounds xs
            FromThenR e1 e2 -> unboundlocals' e1 bounds $
                               unboundlocals' e2 bounds xs
            FromToR e1 e2 -> unboundlocals' e1 bounds $
                             unboundlocals' e2 bounds xs
            FromThenToR e1 e2 e3 -> unboundlocals' e1 bounds $
                                    unboundlocals' e2 bounds $
                                    unboundlocals' e3 bounds xs

freevars :: Exp -> [Name]
freevars e = case e of
 VarE n -> [n]
 ConE _ -> []
 LitE _ -> []
 AppE e1 e2 -> freevars e1 ++ freevars e2
 InfixE m1 op m2 -> concatMap freevars (maybeToList m1 ++ [op] ++ maybeToList m2)
 LamE ps b -> filter (`notElem` concatMap pat2names ps) (freevars b)
 TupE es -> concatMap freevars es
 CondE e1 e2 e3 -> freevars e1 ++ freevars e2 ++ freevars e3
 LetE ds e -> concatMap freevarsDec ds ++ freevars e
 CaseE e ms -> error "unimplemented in Expressions.freevars"
 DoE stms -> error "unimplemented in Expressions.freevars"
 CompE stms -> error "unimplemented in Expressions.freevars"
 ArithSeqE rg -> error "unimplemented in Expressions.freevars"
 ListE es -> concatMap freevars es
 SigE e t -> error "unimplemented in Expressions.freevars"
 RecConE n fs -> error "unimplemented in Expressions.freevars"
 RecUpdE e fs -> error "unimplemented in Expressions.freevars"

freevarsDec :: Dec -> [Name]
freevarsDec (FunD n cs) = n : concatMap freevarsClause cs
freevarsDec (ValD p b ds) = filter (`notElem` pat2names p) (freevarsBody b ++ concatMap freevarsDec ds)
freevarsDec (DataD _ _ _ _ _) = []
freevarsDec (NewtypeD _ _ _ _ _) = []
freevarsDec (TySynD _ _ _) = []
freevarsDec (ClassD _ _ _ _ ds) = concatMap freevarsDec ds
freevarsDec (InstanceD _ _ ds) = concatMap freevarsDec ds
freevarsDec (SigD n _) = [n]
freevarsDec (ForeignD _) = error "in Expressions.freevarsDec"

freevarsClause :: Clause -> [Name]
freevarsClause (Clause ps b ds) =
    filter (`notElem` concatMap pat2names ps) (freevarsBody b ++ concatMap freevarsDec ds)

freevarsBody :: Body -> [Name]
freevarsBody (NormalB e) = freevars e

decname   :: Dec -> [Name]
decname d = case d of
 FunD n _           -> [n]
 ValD p _ _         -> pat2names p
 DataD _ v _ _ _    -> [v]
 NewtypeD _ v _ _ _ -> [v]
 TySynD v _ _       -> [v]
 ClassD _ _ _ _ _   -> []
 InstanceD _ _ _    -> []
 SigD v _           -> [v]
 ForeignD _         -> error "in Yicho.Declarations.decname"

constrname   :: Dec -> [Name]
constrname d = case d of
 FunD n _           -> []
 ValD p _ _         -> []
 DataD _ v _ ds _   -> [v] ++ map f ds
 NewtypeD _ v _ _ _ -> []
 TySynD v _ _       -> []
 ClassD _ _ _ _ _   -> []
 InstanceD _ _ _    -> []
 SigD v _           -> []
 ForeignD _         -> error "in Yicho.Declarations.decname"
 where f (NormalC n _) = n
       f (InfixC _ n _) = n

-- * Pretty Printing
instance Ppr CL where
    ppr (CL e bs fs) = text "CL{" <> ppr e <> text "," <>
                       brackets (hsep (punctuate (text ",") (map ppr bs))) <>
                       text "," <>
                       brackets (hsep (punctuate (text ",") (map ppr fs))) <>
                       text "}"
