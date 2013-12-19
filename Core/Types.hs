-- Core types of PanthR language

module Types where

import Utils
import Env
import Text.Parsec.Pos
-- 
-- instance Show TExpr where
--     show (Typed UndefT expr) =  show expr
--     show (Typed ty expr) =  "(" ++ show expr ++ ": " ++ show ty ++ ")"


intFunFromArithmOp :: ArithmOp -> Integer -> Integer -> Integer
intFunFromArithmOp OpPlus = (+)
intFunFromArithmOp OpMinus = (-)
intFunFromArithmOp OpMult = (*)

dblFunFromArithmOp :: ArithmOp -> Double -> Double -> Double
dblFunFromArithmOp OpPlus = (+)
dblFunFromArithmOp OpMinus = (-)
dblFunFromArithmOp OpMult = (*)
dblFunFromArithmOp OpDivide = (/)






ensureBool :: Value -> Value
ensureBool b@(BoolV _) = b
ensureBool e = error ("Expected a boolean, got: " ++ show e)

valToBool :: Value -> Bool
valToBool (BoolV b) = b
valToBool _ = error "Trying to convert non-boolean to boolean"

-- applyOp :: (Num a) => DuOp -> (a -> a)
applyOp OpGreater = (>)
applyOp OpLess    = (<)
applyOp OpGeq     = (>=)
applyOp OpLeq     = (<=)
applyOp OpEq      = (==)
applyOp OpNeq     = (/=)

-- Checks if its first argument contains a symbol
contains :: Symbol -> Type -> Bool
contains s (VarT s2) = s2 == s
contains s (RecT lst) = any ((contains s) . snd) lst
contains s (VectorT t) = contains s t
contains s (FunT e1 e2) = (contains s e1) && (contains s e2)
contains s _ = False
