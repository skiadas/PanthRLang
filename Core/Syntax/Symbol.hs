module Syntax.Symbol where

import qualified Data.List(nub)

-- Symbols
newtype Symbol = Symbol { toString :: String } deriving (Eq)

instance Show Symbol where
    show (Symbol s) = '\'' : s

makeSymbol :: String -> Symbol
makeSymbol s = Symbol s

uniqueSymbols :: [Symbol] -> Bool
uniqueSymbols lst = lst == Data.List.nub lst


-- Infinite Symbol generators
data SymbolGen = SGen { sym :: Symbol, next :: SymbolGen }

nextGen n = SGen (makeSymbol $ "_X"++show n) (nextGen $ n+1)

seedGen :: SymbolGen
seedGen = nextGen 0

