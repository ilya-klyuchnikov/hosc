{-# OPTIONS -fglasgow-exts -fth #-}
module Examples.MainForWindows where

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
               putStrLn "Higher-order Promotion (Accumulation):"
               prettyYL ex3
               putStrLn "Tupling:"
               prettyYL ex4_1 >> prettyYL ex4_2
               putStrLn "Arithmetic:"
               prettyYL ex5
               putStrLn "Maximum Segment Sum Problem:"
               prettyYL ex6
               putStrLn "Fusing hd an sort:"
               prettyYL ex7
               putStrLn "biggers:"
               mapM_ prettyYL [ex8_1,ex8_2]
               putStrLn "Parallelization:"
               prettyYL ex9
               putStrLn "warm-up transformation:"
               prettyYL ex10_1
               putStrLn "Shortcut deforestation:"
               prettyYL ex11_1
               prettyYL ex11_2
               putStrLn "Maximum Marking Problem:"
               prettyYL mssOpt
               prettyYL mss_even_Opt
               putStrLn "IO swapping:"
               prettyYL ex12_1
               prettyYL ex12_2


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


----------------------------------------------------------------------
-- * Higher-order Promotion (Accumulation)
----------------------------------------------------------------------
{-
  HIGHER-ORDER PROMOTION THEOREM 1 on LISTS
                        e' = \r -> f (e r)
  forall x xs.   a `otimes` (\y -> (f (r y))) = f ((a `oplus` r) acc)
  -------------------------------------------------------------------
            (f .) . foldr oplus e = foldr otimes e'
-}

higherOrderPromotionOnList1 :: RuleY -> RuleY
higherOrderPromotionOnList1 laws exp = do
    [f,e',e,oplus,otimes] <- pvars ["f","e'","e","oplus","otimes"]
    [| ($f .) . foldr $oplus $e |] <=== ret [| $exp |]
    [| $e' |] <=== laws [| \r -> $f ($e r) |]
    [| \a r -> $otimes a (\y -> $f (r y)) |] <=== laws [| \a r acc -> $f ($oplus a r acc) |]
    ret [| foldr $otimes $e' |]

higherOrderPromotionOnList2 :: RuleY -> RuleY
higherOrderPromotionOnList2 laws exp = do
      [psi1,psi2,g,eta1,eta2] <- pvars ["psi1","psi2","g","eta1","eta2"]
      [| foldr $psi2 $psi1 |] <=== ret [| $exp |]
      [| $g |] <=== laws [| $psi1 |]
      [| \a p r -> $eta2 a p ($g r) |] <=== laws [| \a p r -> $psi2 a (\x -> p ($g x)) r |]
      ret [| \xs r -> foldr $eta2 id xs ($g r) |]

applyHigherOrderPromotionOnList :: RuleY -> RuleY
applyHigherOrderPromotionOnList laws exp = do
    [f,g,h] <- pvars ["f","g","h"]
    caseM [| $exp |] [
          [| ($f .) . $g |] ===>
             do g1 <- applyHigherOrderPromotionOnList laws [| $g |]
                g2 <- higherOrderPromotionOnList1 laws [| ($f .) . $g1 |]
                higherOrderPromotionOnList2 laws [| $g2 |],
          [| $h |] ===> promotionOnList laws [| $h . foldr (:) [] |]
                   ]

ex3 = applyHigherOrderPromotionOnList myrules [| (length .) . lrev |]


----------------------------------------------------------------------
-- * Tupling
----------------------------------------------------------------------

tupling2 :: RuleY -> ExpQ -> ExpY
tupling2 laws e = do
    [f,g,oplus,otimes,e1,e2] <- pvars ["f","g","oplus","otimes","e1","e2"]
    [| \x -> ($f x,$g x) |] <=== laws [| $e |]
    [| $e1 |] <=== laws [| $f [] |]
    [| $e2 |] <=== laws [| $g [] |]
    [| \a x -> $oplus   a ($f x, $g x) |] <=== laws [| \a x -> $f (a:x) |]
    [| \a x -> $otimes  a ($f x, $g x) |] <=== laws [| \a x -> $g (a:x) |]
    laws [| foldr (\y (z1,z2) -> ($oplus y (z1,z2),$otimes y (z1,z2))) ($e1,$e2) |]

ex4_1 = tupling2 myrules [| \x -> (sum x, length x) |]
ex4_2 = do sum_length <- tupling2 myrules [| \x -> (sum x, length x) |]
           return [d| new_average x = let (s,l) = $sum_length x
                                       in s `div` l |]

tupling3 :: RuleY -> ExpQ -> ExpY
tupling3 laws e = do
    [e1,e2,e3,f,g,h,oplus,otimes,odot] <- pvars ["e1","e2","e3","f","g","h","oplus","otimes","odot"]
    [| \x -> ($f x,$g x,$h x) |] <=== laws [| $e |]
    [| $e1 |] <=== laws [| $f [] |]
    [| $e2 |] <=== laws [| $g [] |]
    [| $e3 |] <=== laws [| $h [] |]
    [| \a x -> $oplus   a ($f x, $g x, $h x) |] <=== laws [| \a x -> $f (a:x) |]
    [| \a x -> $otimes  a ($f x, $g x, $h x) |] <=== laws [| \a x -> $g (a:x) |]
    [| \a x -> $odot    a ($f x, $g x, $h x) |] <=== laws [| \a x -> $h (a:x) |]
    laws [| foldr (\y (z1,z2,z3) -> ($oplus y (z1,z2,z3),$otimes y (z1,z2,z3),$odot y (z1,z2,z3))) ($e1,$e2,$e3) |]

ex4_3 = prettyYL (tupling3 myrules [| \x -> (steep x, sum x, length x) |])
ex4_4 = prettyYL (do e <- tupling3 myrules [| \x -> (steep x, sum x, length x) |]
                     return [d| faststeep x = fst3 ($e x) |])


------------------------------------
-- * Arithmetic
------------------------------------

factorize exp = do
    c <- pvar "c"
    caseM exp [
      [| $c (\x y -> x ^ 2 - y ^2) |] ===>
         factorize [| $c (\x y -> (x-y) * (x+y)) |],
      [| $c (\x y -> x * y == 0) |] ===>
         factorize [| $c (\x y -> x==0 || y==0) |],
      [| $c (\x y -> x + y == 0) |] ===>
         factorize [| $c (\x y -> x== -y) |],
      [| $c (\x y -> x - y == 0) |] ===>
         factorize [| $c (\x y -> x==y) |],
      ret
              ]

test_arithmetic = do
   let (ps,[x1,x2]) = genPE "x" 2
   e <- factorize [| $x1 ^ 2 - $x2 ^ 2 == 0 |]
   ret (lamE ps e)

ex5 = test_arithmetic


------------------------------------
-- * Maximum Segment Sum Problem
------------------------------------

mutumorphism2 :: RuleY -> ExpQ -> ExpY
mutumorphism2 laws exp = do
    [exp1,exp2,oplus1,e1,oplus2,e2] <- pvars ["exp1","exp2","oplus1","e1","oplus2","e2"]
    [| ($exp1,$exp2) |] <=== laws exp
    [| $e1 |] <=== laws [| $exp1 [] |]
    [| $e2 |] <=== laws [| $exp2 [] |]
    [| \a r -> ($oplus1 a ($exp1 r,$exp2 r)) |] <=== laws [| \a r -> $exp1 (a:r) |]
    [| \a r -> ($oplus2 a ($exp1 r,$exp2 r)) |] <=== laws [| \a r -> $exp2 (a:r) |]
    ret [| mutu2 (($e1,$e2),($oplus1,$oplus2)) |]

flattening2 :: RuleY -> ExpQ -> ExpY
flattening2 laws exp = do
    [e1,e2,oplus1,oplus2] <- pvars ["e1","e2","oplus1","oplus2"]
    [| mutu2 (($e1,$e2),($oplus1,$oplus2)) |] <=== laws exp
    ret [| fst . foldr (\a (r1,r2) -> ($oplus1 a (r1,r2),$oplus2 a (r1,r2))) ($e1,$e2) |]

ex6 = do
   mutu <- mutumorphism2 myrules [| (mss,mis) |]
   flattening2 myrules mutu


------------------------------------
-- * hd . sort
------------------------------------

ex7 = do
  otimes <- myrules [| \a b -> hd (insert a (b:undefined)) |]
  myrules [| foldr $otimes (hd []) |]

------------------------------------
-- * biggers
------------------------------------

-- | for pair
promotion2 :: RuleY -> RuleY
promotion2 laws x = do
      [f,oplus,otimes,e,e',pat] <- pvars ["f","oplus","otimes","e","e'","pat"]
      [| $f . foldr $oplus $e |] <== x
      [| $e' |] <=== laws [| $f $e |]
      pat <=== laws [| \x (z1,z2) -> $otimes x ($f (z1,z2)) |]
      pat <=== laws [| \x (z1,z2) -> $f ($oplus x (z1,z2)) |]
      ret [| foldr $otimes $e' |]

ex8_1 = do
   mutu <- mutumorphism2 myrules [| (biggers,sum) |]
   flattening2 myrules mutu

ex8_2 = do
   [f] <- pvars ["f"]
   [| fst . $f |] <=== ex8_1
   let (pds,[d]) = genPE "d" 1
       (pos,[otimes]) = genPE "otimes" 1
   e1 <- promotion2 myrules [| (foldr $(dyn "d") $otimes `times` id) . $f |]
   ret [| \x -> build (\d -> $(lamE (pds++pos) [| fst ($e1 x) |])) |]


-----------------------------------------------------------
-- * parallelize mis (Maximum Initial segement Sum problem)
-----------------------------------------------------------

distribute_plus_over_max :: RuleY
distribute_plus_over_max exp = do
   [cxt] <- pvars ["cxt"]
   [| $cxt (\ a b c -> a + (b `max` c)) |] <== exp
   ret [| $cxt (\a b c -> (a + b) `max` (a + c)) |]

left_associativity_plus :: RuleY
left_associativity_plus exp = do
   [cxt] <- pvars ["cxt"]
   [| $cxt (\ a b c -> a + (b + c)) |] <== exp
   ret [| $cxt (\a b c -> (a + b) + c) |]

left_associativity_max :: RuleY
left_associativity_max exp = do
   [cxt] <- pvars ["cxt"]
   [| $cxt (\ a b c -> a `max` (b `max` c)) |] <== exp
   ret [| $cxt (\a b c -> (a `max` b) `max` c) |]

ex9 = do
  let ([px1,px2],[x1,x2]) = genPE "x" 2
  let ([py1,py2],[y1,y2]) = genPE "y" 2
  let ([pr],[r]) = genPE "r" 1
  e1 <- myrules [| f $x1 $x2 (f $y1 $y2 $r) |]
  e2 <- distribute_plus_over_max e1
  e3 <- left_associativity_plus e2
  e4 <- left_associativity_max e3
  f' <- myrules [| f |]
  [g1,g2] <- pvars ["g1","g2"]
  pat <- myrules [| \ x1 x2 y1 y2 z -> $f' ($g1 x1 x2 y1 y2) ($g2 x1 x2 y1 y2) z |]
  pat <== lamE [px1,px2,py1,py2,pr] e4
  [e,oplus] <- pvars ["e","oplus"]
  [| foldr $oplus $e |] <=== applyPromotionOnList myrules [| misP |]
  ret [| \x -> let (a1,a2) = h x
                   h (S a) = (a,a)
                   h (x :^: y) = let (x1,x2) <*> (y1,y2) = (x1 `max` (x2+y1), x2+y2)
                                  in h x <*> h y
                in $f' a1 a2 $e |]


------------------------------------
-- * warm-up transformation
------------------------------------

warmup :: RuleY -> RuleY
warmup laws exp = do
   let (ps1,[d]) = genPE "d" 1
       (ps2,[otimes]) = genPE "otimes" 1
   e <- applyPromotionOnList laws [| foldr $otimes $d . $exp |]
   ret [| \x -> build $(lamE (ps2++ps1) [| $e x |]) |]

warmupH :: RuleY -> RuleY
warmupH laws exp = do
   let (ps1,[d]) = genPE "d" 1
       (ps2,[otimes]) = genPE "otimes" 1
   e <- applyPromotionOnList laws [| foldr $otimes $d . $exp |]
   ret [| buildH $(lamE (ps2++ps1) e) |]

ex10_1 = warmup myrules [| map square |]
ex10_2 = warmupH myrules [| map square |]

------------------------------------
-- * shortcut deforestation
------------------------------------

foldr_build x = do
    p <- pvar "p"
    caseM x [
      [| ($p :: forall a d. (forall c. (a -> c -> c) -> c -> (forall b. (a -> b -> b) -> b -> b) -> c) -> d)
         (\k z g -> foldr k z (build g)) |] ==> [| $p (\k z g -> g k z) |]
            ]

ex11_1 = do
    e <- ex10_1
    foldr_build [| \x -> foldr (+) 0 ($e x) |]

foldr_buildH x = do
    p <- pvar "p"
    caseM x [
      [| ($p :: forall a d e. (forall c. (a -> c -> c) -> c ->
                               (forall b. (a -> b -> b) -> b -> ([e] -> b)) -> ([e] -> c)) -> d)
         (\k z g -> foldr k z . buildH g) |] ==> [| $p (\k z g -> g k z) |]
            ]

ex11_2 = do
     e <- ex10_2
     foldr_buildH [| foldr (+) 0 . $e |]


------------------------------------
-- * Maximum Marking Problem
------------------------------------

mmpRule :: RuleY -> ExpQ -> ExpY
mmpRule laws exp = do
   [w,accept,h,oplus,f,phi2,phi1] <- pvars ["w","accept","h","oplus","f","phi2","phi1"]
   [| reduce (bmax $w) . filter conn0 . gen |] <=== laws [| $exp |]
   [| $accept . $h |] <=== tupling_mmp conn0
   [| reduce $oplus . map $f |] <=== ret [| $w |]
   [| foldr $phi2 $phi1 |] <=== applyPromotionOnList laws [| $h |]
   ret [| optMMP ($oplus,$f) $accept $phi1 $phi2 |]

tupling_mmp exp = do
   h <- pvar "h"
   [| $h |] <== [| \xs -> (conn0 xs,conn1 xs,conn2 xs) |]
   ret [| (\ (t0,t1,t2) -> t0) . $h |]

rules_mmp :: RuleY
rules_mmp = mkrules "Examples.Definitions" def_mmp


-- | Maximum Segment Sum Problem
mssOpt = mmpRule rules_mmp [| mss_mmp |]


mmpRule2 :: RuleY -> ExpQ -> ExpY
mmpRule2 laws exp = do
   [w,p,accept,h,oplus,f,phi2,phi1] <- pvars ["w","p","accept","h","oplus","f","phi2","phi1"]
   [| reduce (bmax $w) . filter $p . gen |] <=== laws [| $exp |]
   [| $accept . $h |] <== [| (\ (t0,_,_,_,_,_) -> t0) .
                             (\xs -> ($p xs, even xs, odd xs,conn0 xs,conn1 xs,conn2 xs)) |]
   [| reduce $oplus . map $f |] <=== ret [| $w |]
   [| foldr $phi2 $phi1 |] <=== applyPromotionOnList laws [| $h |]
   ret [| optMMP ($oplus,$f) $accept $phi1 $phi2 |]

rules_mmp2 :: RuleY
rules_mmp2 = mkrules "Examples.Definitions" (liftM2 (++) def_mmp def_mmp2)

-- | Maximum Even-lengh Segment Sum Problem
mss_even_Opt = mmpRule2 rules_mmp2 [| mss_even |]


------------------------------------
-- * IO swapping
------------------------------------

-- No Acc. Param.
ioswap2 dec = do
    [g0,g1,g2,g3] <- pvars ["g0","g1","g2","g3"]
    [d| f1 x = let (rr,mm) = f1' x ($g3 (rr,mm)) in rr
          where f1' y h = case y of
                               [] -> $g0 h
                               a:x' -> let (rr',mm') = f1' x' ($g2 a (rr',mm') h)
                                       in  $g1 a (rr',mm') h |] <== dec
    ret [| let f2 x = let ([], h, r') = f2' (x, $g0 h)
                          f2' ([], (rr,mm)) = (x, $g3 (rr,mm), (rr,mm))
                          f2' (b:y, (rr,mm)) = let (a:x', h', m) = f2' (y, $g1 a (rr,mm) h')
                                               in (x', $g2 a (rr,mm) h', m)
                       in r'
            in fst . f2 |]

ex12_1 = ioswap2 [d| repmincr x = let (r,m) = repc x m in r
                       where repc y h = case y of
                                             [] -> ([],1000)
                                             a:x -> let (r,m') = repc x h
                                                     in (h:r, min a m') |]

-- No Acc. Param.
ioswap1 dec = do
    [g0,g1,g2,g3] <- pvars ["g0","g1","g2","g3"]
    [d| f1 x = let r = f1' x ($g3 r) in r
          where f1' y h = case y of
                               [] -> $g0 h
                               a:x' -> let r' = f1' x' ($g2 a r' h)
                                       in  $g1 a r' h |] <== dec
    ret [| let f2 x = let ([], h, r') = f2' (x, $g0 h)
                          f2' ([], r) = (x, $g3 r, r)
                          f2' (b:y, r) = let (a:x', h', m) = f2' (y, $g1 a r h')
                                         in (x', $g2 a r h', m)
                       in r'
            in f2 |]

ex12_2 = ioswap1 [d| sumCP x = let v = sum' x v in v
                              where sum' y _ = case y of
                                                  []   -> 0
                                                  a:x' -> let v = sum' x' undefined in a + v |]
