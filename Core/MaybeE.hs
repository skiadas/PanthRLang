-- Monad for conditional computation, allowing for errors
module MaybeE
    ( module Control.Applicative
    , module Text.Parsec.Pos
    , ErrorMes
    , MaybeE(..)
    , ok
    , err
    ) where

import Control.Applicative
import Text.Parsec.Pos

newtype ErrorMes = Err (Maybe SourcePos, [String]) deriving (Show)

data MaybeE a = Error ErrorMes | Ok a deriving (Show)

instance Functor MaybeE where
    fmap f (Ok a)     = Ok (f a)
    fmap f (Error m)  = Error m

instance Applicative MaybeE where
    pure a            = Ok a
    (Ok f) <*> (Ok a) = Ok (f a)
    (Error m) <*> _   = Error m
    _ <*> (Error m)   = Error m

instance Monad MaybeE where
    return = pure
    (Ok a)    >>= f = f a
    (Error m) >>= _ = Error m

ok :: a -> MaybeE a
ok a = Ok a

err :: Maybe SourcePos -> [String] -> MaybeE a
err p s = Error (Err (p, s))

