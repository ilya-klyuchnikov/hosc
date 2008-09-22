{-#OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Yicho
--
-- Maintainer  :  yicho@ipl.t.u-tokyo.ac.jp
-- Stability   :  experimental
--
-- Yicho: a combinator library for program transformation.
-- Yicho web site is <http://www.ipl.t.u-tokyo.ac.jp/yicho/>
--
-----------------------------------------------------------------------------

module Yicho (
    -- * Type Declarations
    RuleY,ExpY,

    -- * Basic Combinators
    (<==), (==>), caseM, Choice((<+)),
    -- ** Lifted Match and Lifted Rule
    (<===), (===>),
    -- ** Derivative Combinators
    try,

    -- * Directives
    using, mkrules, mkunfold,
    -- ** Variable Generation
    pvar,pvars,genPE,
    -- ** Beta-reduce
    betareduce,
    -- ** Lifting
    liftY,

    -- * Manipulating Monads
    runY,(@@),
    ret, ret2, retD,

    -- * Pretty Printer
    prettyQ,
    prettyY,
    prettyExpY,
    prettySubstY, prettyExpQ,
    putExpQ, putExpY,
    -- ** Show
    putQ,putY,
    -- ** Light Pretty Printer
    -- | Pretty printing functions ending L print out expressions without module names of variables
    prettyQL, prettyYL, prettyExpYL,
    -- ** Tracer
    traceExpQ, traceExpY, traceExpQL, traceExpYL
                 )
    where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax hiding (lift)
import Yicho.HMatch (hmatch,compatibleCompose,betareduce)
import Yicho.ExpY
import Yicho.Expressions
import Control.Monad.Error (mplus)
import Control.Monad.Error (runErrorT)
import Control.Monad.State (gets,get,put,evalStateT,lift)
import Yicho.Substitutions
import Debug.Trace
import qualified Yicho.Ppr as Ppr

-- | Match
class HMatch a where
    (<==) :: a -- ^ Pattern
          -> a -- ^ Term
          -> Y ()

instance HMatch a => HMatch (Q a) where
    pat <== term = do
       p <- liftY pat  :: Y a
       t <- liftY term :: Y a
       p <== t

instance HMatch a => HMatch [a] where
    ps <== ts = sequence_ $ zipWith (<==) ps ts

instance HMatch Dec where
    pat <== term = dec2exp pat <== dec2exp term

instance HMatch Exp where
    pat <== term = do
       ss <- getSubstY
       let p' = betareduce $ appSubst ss pat
           t' = betareduce $ appSubst ss term
       ss1 <- lift $ hmatch p' t'
       ((s,qdec),ss) <- get
       ss2 <- lift (ss1 `compatibleCompose` ss)
       put ((s,qdec),ss2)

-- | Lifted Match
(<===) :: ExpQ -- ^ Pattern
       -> ExpY -- ^ Lifted term
       -> Y ()
pat <=== term = do term' <- term
                   pat <== term'

-- | Rule
(==>) :: ExpQ -- ^ Pattern
      -> ExpQ -- ^ Expression to be replaced with
      -> RuleY
(pat ==> body) term = (pat <== term) >> (ret2 body)

-- | Lifted Rule
(===>) :: ExpQ -- ^ Pattern
       -> ExpY -- ^ Lifted expression to be replaced with
       -> RuleY
(pat ===> body) term = pat <== term >> body

-- | Meta Case
caseM :: ExpQ -> [RuleY] -> ExpY
caseM sel [] = fail "in caseM"
caseM sel (r:rs) = r sel `mplus` caseM sel rs

class Choice a where
    -- | Deterministic Choice
    (<+) :: a -> a -> a

instance Choice RuleY where
    (r1 <+ r2) x = r1 x `mplus` r2 x

instance Choice ExpY where
    (<+) = mplus

-- | 'try' attempts to apply rule 'r', if possible.  Otherwise, let it be.
try :: RuleY -> RuleY
try r = r <+ ret

-- | Lift from ExpQ to ExpY with instanciating bound variables in environment and
--   unfolding definitions kept in Y monad
ret :: ExpQ -> ExpY
ret e = do e' <- ret2 e
           (n,qds) <- getDecY
           mkrules n qds e'

-- | Lift from ExpQ to ExpY with instanciating bound variables in environment
ret2 :: ExpQ -> ExpY
ret2 e = do e' <- liftY e :: Y Exp
            ss <- getSubstY
            return $ return $ betareduce $ appSubst ss e'

retD :: Q [Dec] -> Y (Q [Dec])
retD d = do d' <- liftY d :: Y [Dec]
            ss <- getSubstY
            return $ return $ betareduce $ appSubst ss d'

-- | Setting definitions together with its module name in Y monad
using :: ExpY -> (String,Q [Dec]) -> ExpY
qe `using` (n,qds) = putDecY (n,qds) >> qe

-- | Make rules from declarations
mkrules :: String -- ^ Module Name
        -> Q [Dec] -- ^ Declarations
        -> RuleY
mkrules mo qdecs qe = do
   decs <- liftY qdecs
   let myrule = mkrules' mo decs myrule
   myrule qe

mkunfold :: String -- ^ Module Name
         -> Q [Dec] -- ^ Declarations
         -> RuleY -> RuleY
mkunfold mo qdecs rule qe = do
   decs <- liftY qdecs
   mkrules' mo decs rule qe

mkrules' :: String -- ^ Module Name
         -> [Dec] -- ^ Declarations
         -> RuleY -> RuleY
mkrules' mo decs rule = out
 where vars   = map (\x -> (mkName x,VarE (mkNameG_v "main" mo x))) $ map nameBase $ concatMap decname decs
       consts = map (\x -> (mkName x,ConE (mkNameG_v "main" mo x))) $ map nameBase $ concatMap constrname decs
       lvar2gvar :: Subst
       lvar2gvar = listToSubst (vars ++ consts)
       out :: RuleY
       out e = do
               c <- pvar "_c" -- dyn "$_c" only provides the single name
               cxt <- liftY c
               let touch :: RuleY -> (Exp,Exp) -> RuleY
                   touch out (e1,e2) = (return (AppE cxt e1) ===> out (return (AppE cxt e2)))
                   rules = map (touch rule) $ {- error $ show $ -} concatMap (mkrules'' mo lvar2gvar) decs
               caseM e (rules ++ [ret2])

mkrules'' :: String                   -- ^ Module Name
          -> Subst                    -- ^ fix name of locally defined functions
          -> Dec                      -- ^ Declaration
          -> [(Exp,Exp)] -- ^ a list of a pair of a pattern and an expression to be replaced with
mkrules'' mo lvar2gvar dec =
    let renamegvars s e = appSubst (s `mapvar` VarE (mkNameG_v "main" mo (nameBase s))) e in
    case dec of
       FunD s cls ->
           let clause2ve (Clause ps (NormalB e) []) =
                   let -- formal parameters of the lambda abstraction of the argument of the context
                       lvars = map VarP (filter (isLocal) (concatMap pat2names ps))
                       -- pattern
                       lhs = foldl1 AppE (VarE s: map pat2exp ps)
                       -- the expression to be replaced with
                       rhs = renamegvars s (simplifyLam e)
                       -- wrap by the context
                       wrap e = simplifyLam (LamE lvars (appSubst lvar2gvar e))
                    in (wrap lhs, wrap rhs)
            in map clause2ve cls
       ValD p (NormalB e) [] ->
           let [v] = pat2names p
               wrap e = appSubst lvar2gvar e
            in [(wrap (VarE v), wrap (renamegvars v e))]
       ValD p _ _ -> error "unimplemented in Yicho.mkrules''.ValD"
       DataD _ _ _ _ _    -> []
       NewtypeD _ _ _ _ _ -> []
       TySynD _ _ _       -> []
       ClassD _ _ _ _ _   -> []
       InstanceD _ _ _    -> []
       SigD _ _           -> []
       ForeignD _         -> error "unimplemented in Yicho.mkrules''"

-- | Monad composition
(@@) :: (Monad m) => (a -> m b) -> (t -> m a) -> t -> m b
f @@ g = \x -> g x >>= f

genPE :: String -> Int -> ([PatQ], [ExpQ])
genPE v n = let ns = [ mkName (v ++ show sn) | sn <- [1..n] ]
             in (map varP ns, map varE ns)

-----------------
-- * for printing

putQ :: Show a => Q a -> IO ()
putQ e = runQ (e >>= runIO . putStrLn . show)

prettyQ :: (Ppr a,Show a) => Q a -> IO ()
prettyQ e = runQ (e >>= runIO . putStrLn . pprint)

prettyQL :: (Ppr.Ppr a,Show a) => Q a -> IO ()
prettyQL e = runQ (e >>= runIO . putStrLn . Ppr.pprint)

putY :: Show a => Y (Q a) -> IO ()
putY e = runQ (runY e >>= runIO . putStrLn . show)

prettyY :: (Ppr a,Show a) => Y (Q a) -> IO ()
prettyY e = runQ (runY e >>= runIO . putStrLn . pprint)

prettyYL :: (Ppr.Ppr a,Show a) => Y (Q a) -> IO ()
prettyYL e = runQ (runY e >>= runIO . putStrLn . Ppr.pprint)

putExpY :: ExpY -> IO ()
putExpY e = runQ (runY e >>= runIO . putStrLn . show)

putExpQ :: ExpQ -> IO ()
putExpQ e = runQ (e >>= runIO . putStrLn . show)

prettyExpY :: ExpY -> IO ()
prettyExpY e = runQ (runY e >>= runIO . putStrLn . pprint)

prettyExpYL :: ExpY -> IO ()
prettyExpYL e = runQ (runY e >>= runIO . putStrLn . Ppr.pprint)

prettyExpQ :: ExpQ -> IO ()
prettyExpQ e = runQ (e >>= runIO . putStrLn . pprint)

prettyExpQL :: ExpQ -> IO ()
prettyExpQL e = runQ (e >>= runIO . putStrLn . Ppr.pprint)

prettySubstY :: Y () -> IO ()
prettySubstY unit = runQ (runY (unit >> getSubstY >>= return . runIO . putStrLn . show))

traceExpQ :: ExpQ -> ExpY
traceExpQ e = do e' <- ret2 e
                 e'' <- liftY e'
                 ret2 $ trace (pprint e'') e

traceExpQL :: ExpQ -> ExpY
traceExpQL e = do e' <- ret2 e
                  e'' <- liftY e'
                  ret2 $ trace (Ppr.pprint e'') e

traceExpY :: ExpY -> ExpY
traceExpY x = do e <- x
                 traceExpQ e

traceExpYL :: ExpY -> ExpY
traceExpYL x = x >>= traceExpQL
