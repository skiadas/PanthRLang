module Syntax.Info where

import Utils
import Syntax.Type
import Text.Parsec.Pos

newtype Info = Info { getInfo :: (Maybe SourcePos, Type) } deriving (Eq)

instance Show Info where
    show (Info (p, t)) = show t

class Sourceable a where
    src :: a -> Maybe SourcePos
    setSrc :: Maybe SourcePos -> a -> a

    hasSrc :: a -> Bool
    hasSrc a = src a /= Nothing


instance Sourceable a => Sourceable (a, b) where
    src      = src . fst
    setSrc t = applyFst (setSrc t)

instance Typable Info where
    typ                     = typ . getInfo
    setTyp ty (Info (p, t)) = Info (p, ty)
-- 
-- instance Sourceable (Maybe SourcePos) where
--     src      = id
--     setSrc s = \_ -> s
-- 
-- instance Sourceable Info where
--     src      = fst . getInfo
--     setSrc p = Info $ (applyFst src) . getInfo

class Infoable i where
    info :: i a -> a
