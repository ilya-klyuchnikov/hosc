{-# OPTIONS -fglasgow-exts -fth #-}
module Examples.Definitions where
import Prelude hiding (sum,map,length,maximum,foldr,fst,snd,id,filter,even,odd)
    -- Functions to be transformed must be defined by users.
    -- Those functions are defined in Definitions0.hs
import Examples.Definitions0

-- Splicing the definitions in Definitions0.hs
$(definitions)

mutu2 ((e1,e2),(oplus1,oplus2)) = let f1 [] = e1
                                      f1 (a:x) = a `oplus1` (f1 x,f2 x)
                                      f2 [] = e2
                                      f2 (a:x) = a `oplus2` (f1 x,f2 x)
                                   in f1

buildH :: forall a c. (forall b. (a -> b -> b) -> b -> ([c] -> b)) -> ([c] -> [a])
buildH g = g (:) []

-- An example of MMP
snd3 (_,b,_) = b
thd3 (_,_,c) = c

type Marked a = (a,Bool)

optMMP :: (Ord b, Num b) =>
          (Int -> b -> b, Marked Int -> Int)
       -> (a -> Bool)
       -> a
       -> (Marked Int -> a -> a)
       -> [Int]
       -> [Marked Int]
optMMP (oplus,f) accept phi1 phi2 xs =
    thd3 (foldr1 (bmax snd3) [ (c,w,r') | (c,w,r') <- foldr psi2 psi1 xs, accept c])
    where psi1 = [(phi1,0,[])]
          psi2 e cand =
              [ (phi2 e' c,f e' `oplus` w,e':r') |
                e' <- [mark e, unmark e],
                (c,w,r') <- cand ]

mss_mmp = reduce (bmax weight) . filter conn0 . gen

conn0 [] = True
conn0 (x:xs) = if marked x then conn1 xs else conn0 xs
conn1 [] = True
conn1 (x:xs) = if marked x then conn1 xs else conn2 xs
conn2 [] = True
conn2 (x:xs) = if marked x then False else conn2 xs

reduce f [x]    = x
reduce f (x:xs) = f x (reduce f xs)
bmax f a b = if f a > f b then a else b
weight = reduce (+) . map fc
fc (x,m) = if marked (x,m) then x else 0

filter p []     = []
filter p (x:xs) = if p x then x : filter p xs
                  else filter p xs

marked (x,b) = b
gen [] = [[]]
gen (x:xs) = [ x':xs' |
               x' <- [mark x,unmark x],
               xs'<- gen xs ]
mark x = (x,True)
unmark x = (x,False)

def_mmp =
 [d|
  mss_mmp = reduce (bmax weight) . filter conn0 . gen
  conn0 [] = True
  conn0 (x:xs) = if marked x then conn1 xs else conn0 xs
  conn1 [] = True
  conn1 (x:xs) = if marked x then conn1 xs else conn2 xs
  conn2 [] = True
  conn2 (x:xs) = if marked x then False else conn2 xs
  weight = reduce (+) . map fc
  |]

even [] = True
even (x:xs) = if marked x then odd xs else even xs
odd [] = False
odd (x:xs) = if marked x then even xs else odd xs
p_conn_even xs = conn0 xs && even xs

def_mmp2 =
 [d|
  mss_even = reduce (bmax weight) . filter p_conn_even . gen
  even [] = True
  even (x:xs) = if marked x then odd xs else even xs
  odd [] = False
  odd (x:xs) = if marked x then even xs else odd xs
  p_conn_even xs = conn0 xs && even xs
  |]

mss_even = reduce (bmax weight) . filter p_conn_even . gen