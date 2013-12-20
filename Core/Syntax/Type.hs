module Syntax.Type where

import Utils
import Syntax.Symbol

data Type = BoolT | IntT | DblT | StrT   -- Base types
          | NumT                         -- Type to encompass all numerical operations
          | RecT [(Symbol, Type)]        -- Records/Tuples
          | VectorT Type
          | FunT Type Type
          | VarT Symbol        -- For type variables, types yet to be determined
          | Top
          | UndefT             -- Undefined type. Non-primitives start that way
          deriving (Eq)

instance Show Type where
    show BoolT        = "Bool"
    show IntT         = "Int"
    show DblT         = "Dbl"
    show StrT         = "Str"
    show NumT         = "Num"
    show (VectorT t)  = "[" ++ show t ++ "]"
    show (RecT _)     = "Rec"    -- FIXME
    show (FunT t1 t2) = show t1 ++ " -> " ++ show t2
    show (VarT s)     = show s
    show Top          = "top"
    show UndefT       = "undef"

class Typable a where
    typ :: a -> Type
    setTyp :: Type -> a -> a

    hasTyp :: a -> Bool
    hasTyp a = typ a == UndefT

instance Typable Type where
    typ       = id
    setTyp ty = \_ -> ty

instance Typable b => Typable (a, b) where
    typ      = typ . snd
    setTyp t = applySnd (setTyp t)

-- Checks if its first argument contains a symbol
contains :: Symbol -> Type -> Bool
contains s (VarT s2) = s2 == s
contains s (RecT lst) = any ((contains s) . snd) lst
contains s (VectorT t) = contains s t
contains s (FunT e1 e2) = (contains s e1) && (contains s e2)
contains s _ = False
