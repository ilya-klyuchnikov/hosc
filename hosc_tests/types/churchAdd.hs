churchAdd where

churchAdd = \m n -> (\f x -> m f (n f x));