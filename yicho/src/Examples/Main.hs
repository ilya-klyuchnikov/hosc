{-# OPTIONS -fglasgow-exts -fth #-}
module Examples.Main where

import Prelude hiding (sum,map,length,maximum,foldr,fst,snd,id,filter,even,odd)
import GHC.Base (build)
import Examples.Definitions
import Examples.Definitions0
import Yicho

import Yicho.ExpY
import Language.Haskell.TH.Lib
import Language.Haskell.TH
import Control.Monad

all_examples = do
               putStrLn "Unfolding (\\x -> sum [1,2,x]):"
               prettyYL ex1_0
               ex1_1
               putStrLn "Definition:"
               prettyYL ex1_2
               prettyQL ex1_3
               putStrLn "Promotion:"
               mapM_ prettyYL [ex2_1,ex2_2,ex2_3,ex2_4,ex2_5,ex2_6,ex2_8]
               prettyYL ex2_7


-- | A rule for unfolding the definition provided by def_sumsq in module "Examples.Definitions"
myrules :: RuleY
myrules = mkrules "Examples.Definitions" definitions

ex1_0 = myrules [| \x -> sum [1,2,x] |]

-- | pretty print the result of the transformation
ex1_1 = prettyYL (myrules [| \x -> sum [1,2,x] |])

-- | run the result of the transformation
{-
  $(runY (myrules [| \x -> sum [1,2,x] |])) 3
-}

-- | define a new definition
ex1_2 = do e <- myrules [| \x -> sum [1,2,x] |]
           return [d| sum123 = $e |]

-- | this is the same definition
ex1_3 = [d| sum123 = $(runY (myrules [| \x -> sum [1,2,x] |])) |]


----------------------------------------------------------------------
-- * Promotion
----------------------------------------------------------------------
-- We can write promotion theorem almost as it is in Haskell.
{-
  PROMOTION THEOREM on LISTS
                 e' = f e
  forall x xs.   x `otimes` f xs = f (x `oplus` xs)
  -------------------------------------------------
       f . foldr oplus e = foldr otimes e'
-}

-- | Promotion Theorem on Lists
promotionOnList :: RuleY -> RuleY
promotionOnList laws exp = do
      [f,oplus,otimes,e,e'] <- pvars ["f","oplus","otimes","e","e'"]
      [| $f . foldr $oplus $e |] <=== laws [| $exp |]
      [| $e' |] <=== laws [| $f $e |]
      [| \x xs -> $otimes x ($f xs) |] <=== laws [| \x xs -> $f ($oplus x xs) |]
      ret [| foldr $otimes $e' |]

ex2_1 = promotionOnList myrules [| map square . foldr (:) [] |]

applyPromotionOnList :: RuleY -> RuleY
applyPromotionOnList laws exp = do
    [f,g,h] <- pvars ["f","g","h"]
    caseM [| $exp |] [
          [| $f . $g |] ===> do g1 <- applyPromotionOnList laws [| $g |]
                                promotionOnList laws [| $f . $g1 |],
          [| $h |] ===> promotionOnList laws [| $h . foldr (:) [] |]
                   ]

ex2_2 = applyPromotionOnList myrules [| map square |]
ex2_3 = applyPromotionOnList myrules [| sum |]
ex2_4 = applyPromotionOnList myrules [| sum . map square |]
ex2_5 = applyPromotionOnList myrules [| length |]
ex2_6 = applyPromotionOnList myrules [| lrev |]
ex2_7 = do e <- applyPromotionOnList myrules [| sum . map square |]
           return [d| sumsq = $e |]
ex2_8 = applyPromotionOnList myrules [| poly |]