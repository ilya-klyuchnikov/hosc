-- This module provides the functions to manipulate the expressions of type
-- ExpY.

module Yicho.ExpY where

import Language.Haskell.TH
import Control.Monad.State
import Control.Monad.Error
import Yicho.Substitutions

type HM e = ErrorT String Q e
-- | Y monad
type Y e = StateT ((String,Q [Dec]),Subst) (ErrorT String Q) e
-- | an expression with the environment and failuire manipulation
type ExpY = Y ExpQ
-- | rule
type RuleY = ExpQ -> ExpY

-- | 'ExpY' -> 'ExpQ' : the inverse of liftY
runY :: Y (Q e) -> Q e
runY e = do e' <- runErrorT (flip evalStateT (("",return []),idSubst) e)
            touch e'
    where touch (Right x) = x
          touch (Left y)  = error ("no solution in runY: " ++ y)

-- | 'ExpQ' -> 'ExpY' : the inverse of 'runY'
liftY :: Q a -> Y a
liftY = lift . lift

-- ** Manipulating the environment
getSubstY :: Y Subst
getSubstY = do (_,ss) <- get
               return ss

getDecY :: Y (String,Q [Dec])
getDecY = do (ds,ss) <- get
             return ds

putDecY :: (String,Q [Dec]) -> Y ()
putDecY x = do (ds,ss) <- get
               put (x,ss)

------------------------------------------------------------
-- * name supply
------------------------------------------------------------

genNames :: Int -> HM [Name]
genNames n = lift (g n)
    where g 0     = return []
          g n     = do x <- newName "v"
                       xs <- g (n-1)
                       return (x:xs)

genvar :: String -> Q ExpQ
genvar s = do n <- newName s
              return (varE n)

-- | Generating a pattern variable
--
-- The following code changes the names each time it is called.
--
-- @
--   pvar = return . join . genvar . ('$':)
-- @
pvar :: String -> Y ExpQ
pvar = liftY . genvar . ('$':)

-- | Generating pattern variables
pvars :: [String] -> Y [ExpQ]
pvars = sequence . map pvar

-- | Generating n pattern variables
pvarN :: Int -> Y [ExpQ]
pvarN 1 = do e <- pvar "z"
             return [e]
pvarN (n+1) = do e <- pvar "z"
                 es <- pvarN n
                 return (e:es)
