module PatternMatch where

import Syntax
import Env

extendBinding :: Env Value -> Pattern -> Value -> Env Value
extendBinding env p v = case (p, v) of
    (VarP s, _) -> extend env s v
    (WildP, _) -> env
    (RecP lstP, RecV lstV) -> foldl (findFieldAndMatch lstV) env lstP
    otherwise -> error "Incompatible pattern match"


findFieldAndMatch :: [(Symbol, Value)] -> Env Value -> (Symbol, Pattern) -> Env Value
findFieldAndMatch [] _ (s, _) = error ("Could not match field " ++ show s)
findFieldAndMatch ((s2, v2):rest) env pat@(s, p) = 
                    if s /= s2
                    then findFieldAndMatch rest env pat
                    else extendBinding env p v2
