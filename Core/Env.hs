-- module implementing functions related to environment lookups

module Env where

import qualified Data.List(nub)
type Symbol = String

toSymbol :: String -> Symbol
toSymbol s = s

type Env a = [(Symbol, a)]

emptyEnv = [] :: Env a

locate :: Env a -> Symbol -> Maybe a
locate [] _                         = Nothing
locate ((s', v):rest) s | s == s'   = Just v
                        | otherwise = locate rest s

extend :: Env a -> Symbol -> a -> Env a
extend env s v = (s,v):env

extendList :: Env a -> [(Symbol, a)] -> Env a
extendList = flip (++)

uniqueSymbols :: [Symbol] -> Bool
uniqueSymbols lst = lst == Data.List.nub lst

unMaybe :: String -> Maybe a -> a
unMaybe s (Just v) = v
unMaybe s Nothing = error s
