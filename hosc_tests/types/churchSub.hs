churchSub where

cpred = \n -> (\f x -> n (\g h -> h (g f)) (\u -> x) (\v -> v));

churchSub = \n m -> (m cpred) n;