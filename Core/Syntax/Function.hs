-- Abstracting functions into their own module, as they interact in a complicated way with Expr and Value
module Syntax.Function where

import Syntax.Symbol
import Syntax.Info

data Function e v a = LambdaE a Symbol (e a) | BuiltInE a (v -> v)

instance Show (Function e v a) where
    show _ = "fun"

instance Functor e => Functor (Function e v) where
    fmap f (LambdaE a s ex) = LambdaE (f a) s (fmap f ex)
    fmap f (BuiltInE a fun) = BuiltInE (f a) fun

instance Infoable (Function e v) where
    info (LambdaE a _ _) = a
    info (BuiltInE  a _) = a
