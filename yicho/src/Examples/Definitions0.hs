{-# OPTIONS -fglasgow-exts -fth #-}
module Examples.Definitions0 where

definitions =
 [d|
  sum (x:xs) = x + sum xs
  sum []     = 0
  map f []     = []
  map f (x:xs) = f x : map f xs
  square x = x * x

  length [] = 0
  length (x:xs) = 1 + length xs
  lrev []    = \r -> r
  lrev (a:x) = \r -> lrev x (a:r)

  fst3 (a,_,_) = a

  steep []    = True
  steep (a:x) = a > average x && steep x
  average x = sum x `div` length x

  poly [] = 0
  poly (a:x) = a + 10 * poly x

  mss [] = 0
  mss (a:x) = a `max` (a + mis x) `max` mss x
  mis [] = 0
  mis (a:x) = a `max` (a + mis x)

  foldr f z []    = z
  foldr f z (a:x) = f a (foldr f z x)

  hd :: [Int] -> Int
  hd []    = negate 9999
  hd (a:x) = a

  sort :: Ord a => [a] -> [a]
  sort = foldr insert []

  insert :: Ord a => a -> [a] -> [a]
  insert x []     = [x]
  insert x (y:ys) = if x >= y then x : insert y ys
                              else y : insert x ys

  biggers [] = []
  biggers (a:x) = if a >= sum x then a : biggers x else biggers x

  misP [] = 0
  misP (a:x) = a `max` (a + misP x)

  data JoinList a = N | S a | JoinList a :^: JoinList a

  f x1 x2 r = x1 `max` (x2 + r)
  f2 x1 x2 x3 x4 x5 si = (x1 `max` (x2 + snd si) `max` (x3 + fst si),
                          x4 `max` (x5 + snd si))
  fst (a,b) = a
  snd (a,b) = b

  (f `times` g) (x,y) = (f x,g y)
  id x = x
  |]