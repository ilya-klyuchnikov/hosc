-- good: only HOSC

data D = F (D -> D);

fix g

where

apply = \x -> case x of { F f -> f; };
fix = \f -> apply (F (\x -> f (apply x x)))(F (\x -> f (apply x x)));