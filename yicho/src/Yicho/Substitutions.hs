{-# OPTIONS -fglasgow-exts -fth #-}
module Yicho.Substitutions where

import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib
import qualified Data.Map
import Yicho.Expressions
import Control.Monad(liftM)
import qualified Yicho.Ppr as Ppr
import qualified Yicho.PprLib as PprLib


-- | Hash table would be better if scaling up
type Subst = Data.Map.Map Name Exp

idSubst :: Subst
idSubst = Data.Map.empty

mapvar :: Name -> Exp -> Subst
p `mapvar` e = Data.Map.singleton p e

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s1 `Data.Map.union` s2

doms :: Subst -> [Name]
doms ss = Data.Map.keys ss

targets :: Subst -> [Exp]
targets s = Data.Map.elems s

filterSubst :: (Name -> Exp -> Bool) -> Subst -> Subst
filterSubst = Data.Map.filterWithKey

removeSubst :: [Name] -> Subst -> Subst
removeSubst strs subst = Data.Map.filterWithKey (\key elm -> key `notElem` strs) subst

maptarget :: (Exp -> Exp) -> Subst -> Subst
maptarget f ss = Data.Map.map f ss

lookupSubst :: Subst -> Name -> Maybe Exp
lookupSubst ss v = Data.Map.lookup v ss

listToSubst :: [(Name,Exp)] -> Subst
listToSubst = Data.Map.fromList

substToList :: Subst -> [(Name,Exp)]
substToList = Data.Map.toList

addToSubst :: Subst -> Name -> Exp -> Subst
addToSubst s n e = Data.Map.insert n e s

intersection :: Subst -> Subst -> Subst
intersection = Data.Map.intersection

nullSubst :: Subst -> Bool
nullSubst = Data.Map.null

-- * Apply Substitution
class Substitutable a where
    appSubst :: Subst -> a -> a

instance Substitutable a => Substitutable [a] where
    appSubst ss xs = map (appSubst ss) xs

instance Substitutable Exp where
    appSubst ss e = case e of
     VarE v -> fromMaybe e (lookupSubst ss v)
     ConE v -> fromMaybe e (lookupSubst ss v) -- for rules
     LitE _ -> e
     AppE e1 e2 -> AppE (appSubst ss e1) (appSubst ss e2)
     InfixE m1 op m2 -> InfixE (fmap (appSubst ss) m1) (appSubst ss op) (fmap (appSubst ss) m2)
     LamE ps e -> let ss' = removeSubst (concatMap pat2names ps) ss
                   in LamE ps (appSubst ss' e)
     TupE es -> TupE (map (appSubst ss) es)
     CondE e1 e2 e3 -> CondE (appSubst ss e1) (appSubst ss e2) (appSubst ss e3)
     LetE ds b ->
         let bs = concatMap decname ds
             ss' = removeSubst bs ss
          in LetE (map (appSubst ss') ds) (appSubst ss b)
     CaseE e ms -> CaseE (appSubst ss e) (appSubst ss ms)
--      DoE stms -> DoE (snd (mapAccumL f ss stms))
--           where f ss stm = let ss' = removeSubst (patvars stm) ss
--                            in (ss',appSubst ss' stm)
--      CompE stms -> CompE (snd (mapAccumL f ss stms))
--          where f ss stm = let ss' = removeSubst (patvars stm) ss
--                            in (ss',appSubst ss' stm)
--      ArithSeqE _ -> e'
     ListE es -> ListE (map (appSubst ss) es)
     SigE e t -> SigE (appSubst ss e) t
--      RecConE _ _ -> e'
--      RecUpdE _ _ -> e'
     e -> error ("in Substitutions.appSubst: "++show e)

instance Substitutable CL where
    appSubst ss (CL exp bs fs) = CL (appSubst ss exp) bs fs

instance Substitutable Clause where
    appSubst ss (Clause ps b ds) =
        let ns = concatMap pat2names ps
            ss' = removeSubst ns ss
         in Clause ps (appSubst ss' b) (map (appSubst ss') ds)

instance Substitutable Dec where
    appSubst ss (FunD n cs) = FunD n (map (appSubst (removeSubst [n] ss)) cs)
    appSubst ss (ValD p b ds) =
        let ns = pat2names p
            ss' = removeSubst ns ss
         in ValD p (appSubst ss' b) (map (appSubst ss') ds)
--    DataD Cxt Name [Name] [Con] [Name]
--    NewtypeD Cxt Name [Name] Con [Name]
--    TySynD Name [Name] Type
--    ClassD Cxt Name [Name] [FunDep] [Dec]
--    InstanceD Cxt Type [Dec]
--    SigD Name Type
--    ForeignD Foreign

instance Substitutable Body where
    appSubst ss (GuardedB ges) = error "unimplemented in Substitutable.appSubst.GuardedB"
    appSubst ss (NormalB e) = NormalB (appSubst ss e)

instance Substitutable a => Substitutable (Q a) where -- for ExpQ
    appSubst = liftM . appSubst

instance Substitutable Match where
    appSubst ss (Match pat body ds) =
        let ss' = removeSubst (pat2names pat) ss
         in Match pat (appSubst ss' body) (appSubst ss ds)

unboundlocalsSubst :: Subst -> [Name]
unboundlocalsSubst ss = concatMap unboundlocals (targets ss)

instance Ppr Subst where
    ppr ss = let ls = hsep $ punctuate (text ",") $ map (\ (n,e) -> ppr n <> text " := " <> ppr e) $ substToList ss
             in braces ls

instance Ppr.Ppr Subst where
    ppr ss = let ls = PprLib.hsep $ PprLib.punctuate (PprLib.text ",") $
                      map (\ (n,e) -> Ppr.ppr n PprLib.<> PprLib.text " := " PprLib.<> Ppr.ppr e) $ substToList ss
             in PprLib.braces ls

isClosedSubst :: (Subst, [Name]) -> Bool
isClosedSubst (ss,bs) = null (filter (`notElem` bs) (unboundlocalsSubst ss))