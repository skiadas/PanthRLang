-- Useful function not found built-in (or didn't look for the)
module Utils where

joinF :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
joinF f g (a,b) = (f a, g b)