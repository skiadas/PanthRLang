module Syntax.Value where

import Syntax.Symbol
import Syntax.Info
import Syntax.Function
import Env
-- Data type for possible values
-- 
-- e is an parametrized type that is to stand for Expr.
-- It is needed for the Function body in a closure, since that contains an expression
-- and we want to avoid mutually recursive modules
data Value e = IntV Integer | DblV Double | BoolV Bool | StrV String
             | VectorV [Value e]
             | ClosV (Env (Value e)) (Function e (Value e) Info)
             | RecV [(Symbol, (Value e))]
             -- deriving (Show)

instance Show (Value a) where
    show _ = "val"
