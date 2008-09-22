{-# OPTIONS -fglasgow-exts -fth #-}
module Yicho.HMatch where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax hiding (lift)
import Yicho.ExpY
import Yicho.Substitutions
import Control.Monad.State
import Yicho.Expressions
import Control.Monad.Error(runErrorT,ErrorT,catchError)
import Data.List(intersect)
import Data.Maybe
import Debug.Trace(trace)
import qualified Yicho.Ppr as YP

-- | Higher-order matching
hmatch :: Exp -- ^ Pattern
       -> Exp -- ^ Term
       -> HM Subst
hmatch p t =
    let ubs = unboundlocals t
     in do p' <- infix2prefix p
           t' <- infix2prefix t
           when (not (linear ubs p')) (error ("non-linear pattern: "++pprint p))
           when (not (flat ubs p')) (error ("non-flat pattern: "++pprint p))
           ss <- hm (CL p' [] ubs) (CL t' [] ubs)
           if isClosedSubst (ss,ubs)
              then return ss
              else fail ("match failed: " ++ pprint ss ++ "\n"++
                         "pattern: " ++ pprint p ++ "\n" ++
                         "term:    " ++ pprint t)

hm :: CL -- ^ Pattern
   -> CL -- ^ Term
   -> HM Subst
hm cl1@(CL {expC=p,boundsC=cs,frozensC=fs})
   cl2@(CL {expC=t,boundsC=bs,frozensC=gs}) = case (p,t) of
 (VarE u, VarE v) | (rigidC cl1 && rigidC cl2) ||
                    (u `elem` fs && v `elem` gs)
                        -> if u == v then return idSubst else fail "in hm VarE"
 (VarE u,      _) | isFreeC cl1 ->
       if null (unboundlocalsC cl2) then return (u `mapvar` t)
       else fail ("in hm: the term contains unboundlocal variables: "++ show (unboundlocalsC cl2) ++"\n"++
                  "pattern: "++show cl1++"\n"++
                  "term:    "++show cl2)
 (ConE u, ConE v) -> if nameBase u == nameBase v &&
                        nameModule u == nameModule v &&
                        isJust (nameModule u) &&
                        isJust (nameModule v)
                     then return idSubst else fail "in hm ConE"
 (LitE u, LitE v) -> if u == v then return idSubst else fail "in hm LitE"

 (AppE p1 p2, _) | flexC cl1 ->
  do let (hd,args) = headargs p
     ns <- genNames (length args)
     let localvars = concatMap outmostlocalvars args
     if detargs (cs,fs) args
        then do -- DHP3
                let (tup,mktup) = splitTup (getBody p2)
                    froms = map (\m -> CL m cs fs) tup
                ms <- genNames (length froms)
                let tos   = map (\m -> CL (foldl1 AppE (VarE m : map VarE (outmostlocalvars p2))) bs gs) ms
                (_,ss) <- dreplaces (cl2 {boundsC=ns++bs,frozensC=ns++unboundlocals t++gs}) (zip froms tos)
                (CL e bs gs',_) <- dreplaces (cl2 {boundsC=ns++bs,frozensC=ns++unboundlocals t++gs}) (zip froms (map (appSubst ss) tos))
                ss1 <- hm (CL p1 cs fs) (CL ({- appSubst ss -} (simplifyLam (LamE [mktup ms] e))) bs gs')
                ss2 <- ss `compatibleCompose` ss1
                return $ removeSubst localvars ss2 -- Removing the bindings of local variables
        else if validLTargs cs args
        then do (e',(rs,ss1)) <- runStateT (replacesLT (CL t bs gs))
                                           (map (patternize cs) (zip args (map VarE ns)),idSubst)
                let ss1' = removeSubst localvars ss1 -- Removing the bindings of local variables
                case rs of
                        [] -> do ss2 <- hm (CL hd cs fs)
                                           (CL (simplifyLam (LamE (map VarP ns)
                                                             (appSubst ss1 e'))) bs gs)
                                 ss1' `compatibleCompose` ss2
                        x  -> fail ("hm fails in flexible case: "++show x++"\n"++
                                    "pattern: "++pprint cl1++"\n"++
                                    "term:    "++pprint cl2)
        else error ("Matching algorithm for this pattern is not implemented in HMatch.hm: "++pprint p)
 (AppE p1 p2, AppE t1 t2) | not (flexC (CL p1 cs fs)) ->
     do ss1 <- hm (CL p1 cs fs) (CL t1 bs gs)
        ss2 <- hm (CL p2 cs fs) (CL (appSubst ss1 t2) bs gs)
        compatibleCompose ss1 ss2
 (LamE (VarP p:ps) e, LamE (VarP q:qs) f) ->
     do x <- lift $ newName "_x"
        let pat  = simplifyLam $ appSubst (p `mapvar` VarE x) (LamE ps e)
            term = simplifyLam $ appSubst (q `mapvar` VarE x) (LamE qs f)
        hm (CL pat (x:cs) fs) (CL term (x:bs) gs)
 (LamE (VarP p:ps) e, LamE (WildP:qs) f) ->
     do x <- lift $ newName "_x"
        let pat  = simplifyLam $ appSubst (p `mapvar` VarE x) (LamE ps e)
            term = simplifyLam (LamE qs f)
        hm (CL pat (x:cs) fs) (CL term bs gs)
 (LamE (WildP:ps) e, LamE (VarP q:qs) f) ->
     do x <- lift $ newName "_x"
        let pat  = simplifyLam (LamE ps e)
            term = simplifyLam $ appSubst (q `mapvar` VarE x) (LamE qs f)
        hm (CL pat (x:cs) fs) (CL term bs gs)
 (LamE (WildP:ps) e, LamE (WildP:qs) f) ->
        hm (CL (simplifyLam (LamE ps e)) cs fs) (CL (simplifyLam (LamE qs f)) bs gs)
 (LamE (VarP p:ps) e, f) ->
     do x <- lift $ newName "x"
        let pat  = simplifyLam $ appSubst (p `mapvar` VarE x) (LamE ps e)
            term = simplifyLam $ AppE f (VarE x)
        hm (CL pat (x:cs) fs) (CL term (x:bs) gs)
 (LamE (TupP p:ps) e, LamE (TupP q:qs) f) | length p == length q ->
        hm (CL (LamE (p++ps) e) cs fs)
           (CL (LamE (q++qs) f) bs gs)
 (LamE (ConP c1 pas:ps) e, LamE (ConP c2 eas:qs) f) ->
     if nameBase c1 == nameBase c2 && nameModule c1 == nameModule c2 &&
        isJust (nameModule c1) && isJust (nameModule c2) &&
        length pas == length eas
        then hm (CL (LamE (pas++ps) e) cs fs) (CL (LamE (eas++qs) f) bs gs)
        else fail "in hm LamE.ConP"
 (LamE (InfixP p1 n p2:ps) e, LamE (InfixP q1 m q2:qs) f) ->
     if nameBase n == nameBase m && nameModule n == nameModule m &&
        isJust (nameModule n) && isJust (nameModule m)
        then hm (CL (LamE (p1:p2:ps) e) cs fs) (CL (LamE (q1:q2:qs) f) bs gs)
        else fail "in hm LamE.InfixP"
 (LamE [] e,f) -> hm (CL e cs fs) (CL f bs gs)
 (LamE ps e,f) -> error ("not implemented in HMatch.LamE: "++ show (LamE ps e))
 (TupE ps, TupE ts) -> do let f [] [] = return idSubst
                              f (p:ps) (t:ts) = do ss1 <- hm (CL p cs fs) (CL t bs gs)
                                                   ss2 <- f ps ts
                                                   ss1 `compatibleCompose` ss2
                          f ps ts
 (CondE e1 e2 e3, CondE f1 f2 f3) ->
     do ss1 <- hm (CL e1 cs fs) (CL f1 bs gs)
        ss2 <- hm (CL e2 cs fs) (CL (appSubst ss1 f2) bs gs)
        ss2' <- compatibleCompose ss1 ss2
        ss3 <- hm (CL e3 cs fs) (CL (appSubst ss2 f3) bs gs)
        compatibleCompose ss2 ss3
 (LetE ds1 e1,LetE ds2 e2) -> do
      let cs1 = concatMap decname ds1
          bs1 = concatMap decname ds2
          rename [] (e1,e2,ls) = return (e1,e2,ls)
          rename ((a,b):cbs) (e1,e2,ls) = do
             x <- lift $ newName "x"
             let e1' = simplifyLam $ appSubst (a `mapvar` VarE x) e1
                 e2' = simplifyLam $ appSubst (b `mapvar` VarE x) e2
             rename cbs (e1',e2',x:ls)
          {- rename::
            (let r = r in r,let r' = r' in r',[]) ==> (let r = r in x,let r' = r' in x,[x])
          -}
          f [] [] = return idSubst
          f (p:ps) (t:ts) = do ss1 <- hm (CL p cs fs) (CL t bs gs)
                               ss2 <- f ps ts
                               ss1 `compatibleCompose` ss2
      (LetE ds1' e1',LetE ds2' e2',ls) <- rename (zip cs1 bs1) (LetE ds1 e1,LetE ds2 e2,[])
      ss3 <- f (map dec2exp ds1') (map dec2exp ds2')
      ss4 <- hm (CL e1' (ls++cs) fs) (CL e2' (ls++bs) gs)
      compatibleCompose ss3 ss4
 (CaseE e ms,CaseE f ns) -> do
     ss1 <- hm (CL e cs fs) (CL f bs gs)
     let f (e,f) = hm (CL e cs fs) (CL f bs gs)
     sss <- mapM f (zip (map match2exp ms) (map match2exp ns))
     ss2 <- foldM compatibleCompose idSubst sss
     compatibleCompose ss1 ss2
 (DoE _,_)       -> error "not implemented in hm"
 (_,DoE _)       -> error "not implemented in hm"
 (CompE _,_)     -> error "not implemented in hm"
 (_,CompE _)     -> error "not implemented in hm"
 (ArithSeqE _,_) -> error "not implemented in hm"
 (_,ArithSeqE _) -> error "not implemented in hm"
 (ListE ps, ListE ts) -> do let f [] [] = return idSubst
                                f (p:ps) (t:ts) = do ss1 <- hm (CL p cs fs) (CL t bs gs)
                                                     ss2 <- f ps ts
                                                     ss1 `compatibleCompose` ss2
                            f ps ts
 (SigE p t, f) -> hm (CL p cs fs) cl2
 (p, SigE f _) -> hm cl1 (CL f cs gs)
 (RecConE _ _,_) -> error "not implemented in hm"
 (_,RecConE _ _) -> error "not implemented in hm"
 (RecUpdE _ _,_) -> error "not implemented in hm"
 (_,RecUpdE _ _) -> error "not implemented in hm"
 (_,_) -> fail ("not match in hm\n"++
                "pattern: " ++ pprint cl1 ++ "\n" ++
                "term   : " ++ pprint cl2)

-- | Converting an infix expression into a prefix expression
class Infix2Prefix a where
    infix2prefix :: a -> HM a

instance Infix2Prefix Exp where
    -- infix2prefix e | trace (pprint e) False = undefined
    infix2prefix e@(VarE _) = return e
    infix2prefix e@(ConE _) = return e
    infix2prefix e@(LitE _) = return e
    infix2prefix (AppE e1 e2) = do
       e1' <- infix2prefix e1
       e2' <- infix2prefix e2
       return (AppE e1' e2')
    infix2prefix (InfixE Nothing e Nothing) = infix2prefix e
    infix2prefix (InfixE (Just a) e Nothing) = do
       a' <- infix2prefix a
       e' <- infix2prefix e
       return (AppE e' a')
    infix2prefix (InfixE Nothing e (Just b)) = do
        x <- lift (newName "x")
        b' <- infix2prefix b
        return (LamE [VarP x] (AppE (AppE e (VarE x)) b'))
    infix2prefix (InfixE (Just a) e (Just b)) = do
        a' <- infix2prefix a
        b' <- infix2prefix b
        e' <- infix2prefix e
        return (AppE (AppE e' a') b')
    infix2prefix (LamE ps b) = do
        b' <- infix2prefix b
        return (LamE ps b')
    infix2prefix (TupE es) = do
        es' <- mapM infix2prefix es
        return (TupE es')
    infix2prefix (CondE e1 e2 e3) = do
        e1' <- infix2prefix e1
        e2' <- infix2prefix e2
        e3' <- infix2prefix e3
        return (CondE e1' e2' e3')
    infix2prefix (LetE ds e)  = do
        ds' <- infix2prefix ds
        e'  <- infix2prefix e
        return (LetE ds' e')
    infix2prefix (CaseE e ms) = do
        e'  <- infix2prefix e
        ms' <- infix2prefix ms
        return (CaseE e' ms')
    infix2prefix (CompE stms) = error "unimplemented in HMatch.infix2prefix.CompE"
    infix2prefix (ArithSeqE rg) = error "unimplemented in HMatch.infix2prefix.ArithSeqE"
    infix2prefix (ListE es) = do es' <- mapM infix2prefix es
                                 return (ListE es')
    infix2prefix (SigE e t) = do e' <- infix2prefix e
                                 return (SigE e' t)
    infix2prefix (RecConE _ fs) = error "unimplemented in HMatch.infix2prefix.RecConE"
    infix2prefix (RecUpdE e fs) = error "unimplemented in HMatch.infix2prefix.RecUpdE"

instance Infix2Prefix Dec where
    infix2prefix (FunD name cls) = do
        cls' <- infix2prefix cls
        return (FunD name cls')
    infix2prefix (ValD pat body ds) = do
        body' <- infix2prefix body
        ds'   <- infix2prefix ds
        return (ValD pat body' ds')

instance Infix2Prefix a => Infix2Prefix [a] where
    infix2prefix es = mapM infix2prefix es

instance Infix2Prefix Clause where
    infix2prefix (Clause ps body ds) = do
        body' <- infix2prefix body
        ds'   <- infix2prefix ds
        return (Clause ps body' ds')

instance Infix2Prefix Body where
    infix2prefix (NormalB e) = do
        e' <- infix2prefix e
        return (NormalB e')

instance Infix2Prefix Match where
    infix2prefix (Match pat body ds) = do
        body' <- infix2prefix body
        ds'   <- infix2prefix ds
        return (Match pat body' ds')

data ArgStatus = ArgStatus {from::Exp,to::Exp} deriving Show -- Is Closure better?

patternize :: [Name]
           -> (Exp, Exp) -- ^ (from,to)
           -> ArgStatus
patternize cs (LamE ps e,f) = ArgStatus { from =e, to=foldl1 AppE (f : map pat2exp ps) }
patternize cs (e,f) = ArgStatus { from =e, to=f }

detargs :: ([Name],[Name]) -> [Exp] -> Bool
detargs (cs,fs) args =
    all containlocal args && all secondOrderArg args &&
    not (intersect args) && all appearsin args
    where -- containing unbound variables which is bound in pattern
          containlocal e = let ubs = filter (`notElem` fs) (unboundlocals e)
                            in not (null ubs) && null (filter (`notElem` cs) ubs)
          intersect :: [Exp] -> Bool
          intersect es = let es' = zip es [1..]
                             isBox (VarE v) bs = v `elem` bs
                             isBox _ _ = False
                          in or [ eqBox (lambdaBody a,outmostlocalvars a)
                                        (b',outmostlocalvars b)
                                        cs |
                                  (a,n) <- es',
                                  not (isBox (lambdaBody a) (outmostlocalvars a)),
                                  (b,_) <- drop n es',
                                  b' <- subexps b ]
                          || or [ eqBox (a',outmostlocalvars a)
                                        (lambdaBody b,outmostlocalvars b)
                                        cs |
                                  (a,n) <- es',
                                  (b,_) <- drop n es',
                                  not (isBox (lambdaBody b) (outmostlocalvars b)),
                                  a' <- subexps a ]

-- | Whether are they valid arguments for A_{LT} patterns
validLTargs :: [Name] -> [Exp] -> Bool
validLTargs cs args = all secondOrderArg args && all appearsin args

-- | @p (\x y -> body)@   we assume body does not contain lambda abstraction
replacesLT :: CL -> StateT ([ArgStatus],Subst) (ErrorT String Q) Exp
replacesLT cl@(CL {expC=e,boundsC=bs,frozensC=fs}) =
    do (rs0,ss0) <- get
       if null rs0 then return e
          else do e' <- replaceLT cl
                  (rs,ss) <- get
                  if e' /= e then d (appSubst ss e')
                     else d e'
    where
    d e@(VarE x) = return e
    d e@(ConE _) = return e
    d e@(LitE _) = return e
    d (AppE f e) = do f' <- replacesLT (CL f bs fs)
                      e' <- replacesLT (CL e bs fs)
                      return (AppE f' e')
    d (InfixE m1 e m2) = do
                         m1' <- case m1 of
                                        Nothing -> return Nothing
                                        Just a -> do a' <- replacesLT (CL a bs fs)
                                                     return (Just a')
                         e' <- replacesLT (CL e bs fs)
                         m2' <- case m2 of
                                        Nothing -> return Nothing
                                        Just a -> do a' <- replacesLT (CL a bs fs)
                                                     return (Just a')
                         return (InfixE m1' e' m2')
    d (LamE ps e) = do let ns = concatMap pat2names ps -- correct?
                       e' <- replacesLT (CL e (ns ++ bs) (ns ++ fs))
--                       e' <- replacesLT (CL e (ns ++ bs) fs)
                       return (LamE ps e')
    d (TupE es) = do es <- sequence [ replacesLT (CL e bs fs) | e <- es ]
                     return (TupE es)
    d (LetE ds e) = do let ns = concatMap decname ds -- need to check ds
                       e' <- replacesLT (CL e (ns++bs) (ns++fs))
                       return (LetE ds e')
    d (CondE e1 e2 e3) = do e1' <- replacesLT (CL e1 bs fs)
                            e2' <- replacesLT (CL e2 bs fs)
                            e3' <- replacesLT (CL e3 bs fs)
                            return (CondE e1' e2' e3')
    d (CaseE _ _)   = error $ show ("not implemented in HMatch.d: "++show e)
    d (DoE _)       = error $ show ("not implemented in HMatch.d: "++show e)
    d (CompE _)     = error $ show ("not implemented in HMatch.d: "++show e)
    d (ArithSeqE _) = error $ show ("not implemented in HMatch.d: "++show e)
    d (ListE _)     = error $ show ("not implemented in HMatch.d: "++show e)
    d (SigE _ _)    = error $ show ("not implemented in HMatch.d: "++show e)
    d (RecConE _ _) = error $ show ("not implemented in HMatch.d: "++show e)
    d (RecUpdE _ _) = error $ show ("not implemented in HMatch.d: "++show e)

replaceLT :: CL -> StateT ([ArgStatus],Subst) (ErrorT String Q) Exp
replaceLT cl@(CL {expC=e,boundsC=bs,frozensC=fs}) =
    do (rs,ss) <- get
       case rs of
               [] -> return e
               (st:rs') -> do let (t,e') = (from st,to st)
                              ss' <- lift (hm (CL t bs []) (CL e bs fs))
                              ss'' <- lift (ss `compatibleCompose` ss')
                              put (rs',ss'')
                              lift $ return e'
                           `catchError` (\_ -> return e)

-- Return a composed substitution if the input substitutions are compatible
compatibleCompose :: Subst -> Subst -> HM Subst
compatibleCompose ss1 ss2 =
    let ks = doms ss1 `intersect` doms ss2
     in if null ks
        then return (ss1 `compose` ss2)
        else do msum (zipWith hmatch (map (appSubst ss1.VarE) ks) (map (appSubst ss2.VarE) ks))
                return (ss1 `compose` ss2)

dreplaces :: CL -> [(CL,CL)] -> HM (CL,Subst)
dreplaces cl []     = return (cl,idSubst)
dreplaces cl (r:rs) = do (cl1,ss1) <- dreplace cl r
                         (cl2,ss2) <- dreplaces cl1 rs
                         ss3 <- ss1 `compatibleCompose` ss2
                         return (cl2,ss3)

-- replace (pat,cs) with (rep,cs)
dreplace :: CL -> (CL,CL) -> HM (CL,Subst)
dreplace cl1@(CL {expC=e,boundsC=bs,frozensC=gs}) (f,t) =
    do ss <- hm f cl1
       return (t,ss)
    `catchError` (\_ -> d e)
    where
    d e@(VarE _) = return (cl1,idSubst)
    d e@(ConE _) = return (cl1,idSubst)
    d e@(LitE _) = return (cl1,idSubst)
    d (AppE a b) = do (cla,ssa) <- dreplace (cl1 { expC=a }) (f,t)
                      (clb,ssb) <- dreplace (cl1 { expC=b }) (f,t)
                      ss' <- ssa `compatibleCompose` ssb
                      return (CL (AppE (expC cla) (expC clb)) bs gs, ss')
    d e@(InfixE m1 op m2) = error "impossible happened in HMatch.dreplace.d"
    d (LamE ps e) = do let ns = concatMap pat2names ps
                       (cl, ss) <- dreplace (cl1 { expC=e, boundsC=ns++bs, frozensC=ns++gs }) (f,t)
                       return (cl { expC = LamE ps (expC cl) },ss)
    d (TupE es) = do css <- sequence [ dreplace (cl1 {expC=e}) (f,t) | e <- es ]
                     let f (es,ss1) (cl,ss2) = do ss' <- ss1 `compatibleCompose` ss2
                                                  return (expC cl : es,ss')
                     (es',ss') <- foldM f ([],idSubst) css
                     return (cl1 {expC=TupE (reverse es')},ss')
    d (CondE e1 e2 e3) = do (cla,ss1) <- dreplace (cl1 { expC=e1 }) (f,t)
                            (clb,ss2) <- dreplace (cl1 { expC=e2 }) (f,t)
                            (clc,ss3) <- dreplace (cl1 { expC=e3 }) (f,t)
                            ss2' <- ss1 `compatibleCompose` ss2
                            ss' <- ss2' `compatibleCompose` ss3
                            return (CL (CondE (expC cla) (expC clb) (expC clc)) bs gs, ss')
    d (LetE ds e) = do (cla,ss') <- dreplace (cl1 { expC=e }) (f,t)
                       return (CL (LetE ds (expC cla)) bs gs, ss') -- fix
    d (CaseE _ _)   = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (DoE _)       = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (CompE _)     = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (ArithSeqE _) = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (ListE _)     = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (SigE _ _)    = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (RecConE _ _) = error $ show ("not implemented in HMatch.dreplace.d: "++show e)
    d (RecUpdE _ _) = error $ show ("not implemented in HMatch.dreplace.d: "++show e)


-------------
-- betareduce

class Betareduce a where
    betareduce :: a -> a

instance Betareduce Exp where
  betareduce e = beta e
    where
    beta e = case e of
     LamE ps b -> etaRed $ simplifyLam $ LamE ps (beta b) -- Fix me!!
     VarE _ -> e
     ConE _ -> e
     LitE _ -> e
     AppE (AppE e1 e2) e3 | isOperator (beta e1) ->
          InfixE (Just (beta e2)) (beta e1) (Just (beta e3))
     AppE e1 e2 | isOperator (beta e1) ->
          InfixE (Just (beta e2)) (beta e1) Nothing
     AppE (AppE (InfixE Nothing e1 Nothing) e2) e3 ->
          InfixE (Just (beta e2)) (beta e1) (Just (beta e3))
     AppE (InfixE (Just e1) op Nothing) e2 ->
          InfixE (Just (beta e1)) (beta op) (Just (beta e2))
     AppE (InfixE Nothing op Nothing) e1 ->
          InfixE (Just (beta e1)) (beta op) Nothing
     AppE (VarE n1) e2 | n1 == mkNameG_v "base" "GHC.Err" "undefined" -> VarE (mkNameG_v "base" "GHC.Err" "undefined")
     AppE (AppE (InfixE Nothing op m2) e1) e2 ->
         beta (AppE (InfixE (Just e1) op m2) e2)
     AppE e1@(VarE v) _ | mkNameG_v "base" "GHC.Err" "undefined" == v -> e1
     AppE e1 (CondE b1 b2 b3) -> CondE b1 (AppE e1 b2) (AppE e1 b3) -- Fix me!
     AppE (CondE b1 b2 b3) e2 -> CondE b1 (AppE b2 e2) (AppE b3 e2) -- Fix me!
     AppE e1 e2 ->
         case e1' of
              LamE [] b     -> error "LamE in beta"
              LamE (p:ps) b ->
                  let match = patexpmatch p e2'
                   in case match of
                           Nothing -> AppE e1' e2'
                           Just m  -> beta (LamE ps (appSubst m b))
--               InfixE Nothing op m2 ->
--                   InfixE (Just e2') (beta op) (fmap beta m2)
--               InfixE m1 op Nothing ->
--                   InfixE (fmap beta m1) (beta op) (Just e2')
              _               -> AppE e1' e2'
         where e1' = beta e1
               e2' = beta e2
     InfixE (Just e1) op (Just e2) ->
         let e1' = beta e1; op' = beta op; e2' = beta e2
          in if isOperator op' then InfixE (Just e1') op' (Just e2') -- AppE (AppE op' e1') e2'
             else AppE (AppE op' e1') e2'
     InfixE Nothing op Nothing -> beta op
     InfixE (Just e1) op Nothing -> InfixE (Just (beta e1)) (beta op) Nothing
     InfixE Nothing op (Just e2) -> InfixE Nothing (beta op) (Just (beta e2))
                                    -- AppE (AppE (VarE (mkNameG_v "base" "GHC.Base" "flip")) op) e2
     -- LamE moved before ``undefined'' case
     TupE es -> TupE (map beta es)
     CondE e1 e2 e3 -> CondE (beta e1) (beta e2) (beta e3)
     LetE ds e -> LetE (betareduce ds) (beta e)
     CaseE e ms -> CaseE (beta e) (betareduce ms)
--      DoE stmts -> e'
--      CompE stmts -> e'
--      ArithSeqE range -> e'
     ListE es -> foldr (AppE . AppE (ConE (mkNameG_v "base" "GHC.Base" ":")))
                       (ConE (mkNameG_v "base" "GHC.Base" "[]"))
                       (map beta es)
     SigE e t -> SigE (beta e) t
     RecConE _ _ -> error $ "not implemented in HMatch.beta: " ++ show e
     RecUpdE _ _ -> error $ "not implemented in HMatch.beta: " ++ show e
     e           -> error $ "not implemented in HMatch.beta: " ++ show e

instance Betareduce a => Betareduce [a] where
    betareduce = map betareduce

instance Betareduce Dec where
    betareduce (FunD name cls) = FunD name (betareduce cls)
    betareduce (ValD pat body ds) = ValD pat (betareduce body) (betareduce ds)

instance Betareduce Clause where
    betareduce (Clause ps body ds) = Clause ps (betareduce body) (betareduce ds)

instance Betareduce Body where
    betareduce (NormalB e) = NormalB (betareduce e)

instance Betareduce Match where
    betareduce (Match pat body ds) = Match pat (betareduce body) (betareduce ds)

-- | For beta reduction
patexpmatch             :: Pat -> Exp -> Maybe Subst
patexpmatch (LitP l1) (LitE l2)
    | l1 == l2          = Just idSubst
patexpmatch (VarP v1) e = Just (v1 `mapvar` e)
patexpmatch (TupP ps) (TupE es) -- assume linear patterns
    = fmap (foldr compose idSubst) $ sequence $ zipWith patexpmatch ps es
patexpmatch (TupP ps) x = Nothing
patexpmatch (ConP s []) (ConE t)
    | s == t    = Just idSubst
    | otherwise = Nothing
patexpmatch (ConP s ps@(_:_)) (AppE e1 e2) =
    liftM2 compose (patexpmatch (ConP s (init ps)) e1) (patexpmatch (last ps) e2)
patexpmatch (ConP s _) _ = Nothing
patexpmatch (TildeP p) e = patexpmatch p e
patexpmatch (AsP x p) e = do ss <- patexpmatch p e
                             Just (addToSubst ss x e)
patexpmatch WildP _ = Just idSubst
patexpmatch (RecP name1 fps) (RecConE name2 fes)
    | name1 == name2 = fieldmatch fps fes
    where
    fieldmatch [] fes = Just idSubst
    fieldmatch ((v,pat):ps) fes
      = case filter ((==v).fst) fes of
             [(_,exp)] -> liftM2 compose (patexpmatch pat exp) (fieldmatch ps fes)
             _         -> Nothing
patexpmatch (ListP ps) (ListE es)
    | length ps == length es = fmap (foldr compose idSubst) $ sequence $ zipWith patexpmatch ps es
patexpmatch (ListP []) (ConE n) | n == mkNameG_v "base" "GHC.Base" "[]" = Just idSubst
patexpmatch (ListP (p:ps)) (InfixE (Just e1) (ConE n) (Just e2)) | n == mkNameG_v "base" "GHC.Base" "[]" =
    liftM2 compose (patexpmatch p e1) (patexpmatch (ListP ps) e2)
patexpmatch (ListP (p:ps)) (AppE (AppE (ConE c) e1) e2) | c == mkNameG_v "base" "GHC.Base" ":" =
    liftM2 compose (patexpmatch p e1) (patexpmatch (ListP ps) e2)
patexpmatch (ListP _) _ = Nothing
patexpmatch pat term = error $ "in patexpmatch: " ++ show (pat,term)
