module Syntax.Pattern where

import Utils
import Syntax.Type
import Syntax.Info
import Syntax.Symbol

-- Patterns
data Pattern a = VarP a Symbol                  -- Variable
               | RecP a [(Symbol, Pattern a)]   -- Record / Tuple
               | WildP a                        -- Wildcard
               -- deriving (Show)

instance Show (Pattern a) where
    show (VarP _ s)   = show s
    show (RecP _ lst) = "{" ++ foldr (++) "" (map show lst) ++ "}"
    show (WildP _)    = "_"

instance Infoable Pattern where
    info (VarP a _) = a
    info (RecP a _) = a
    info (WildP a)  = a

instance Typable a => Typable (Pattern a) where
    typ      = typ . info
    setTyp t = fmap (setTyp t)

instance Sourceable a => Sourceable (Pattern a) where
    src = src . info
    setSrc s = fmap (setSrc s)

instance Functor Pattern where
    fmap f p = case p of
        (VarP a s)   -> VarP (f a) s
        (RecP a lst) -> RecP (f a) (map (applySnd $ fmap f) lst)
        (WildP a)    -> WildP (f a)


makeTuplePat :: a -> [Pattern a] -> Pattern a
makeTuplePat a lst = RecP a $ zip (map (makeSymbol . show) [1..(length lst)]) lst
-- 
makeRecPat :: a -> [(String, Pattern a)] -> Pattern a
makeRecPat a lst = RecP a $ map (applyFst makeSymbol) lst
-- 
makeVarPat :: a -> String -> Pattern a
makeVarPat a = (VarP a) . makeSymbol
