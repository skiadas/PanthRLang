module Unify (unify) where

import Utils
import Types
import Env
import TypeCheckTypes

-- Applies constraint unification.
-- Parameter is the remaining constraints
-- Return value is the resolved constraints
unify :: Constraints -> Context
unify [] = emptyContext
unify ((x,y):rest) =
    if x == y
    then unify rest
    else case (x, y) of
        (IntT, NumT) -> unify rest
        (DblT, NumT) -> unify rest
        (NumT, IntT) -> unify rest
        (NumT, DblT) -> unify rest
        (VarT _, NumT) -> unify rest -- FIXME
        (_, UndefT) -> unify rest
        (UndefT, _) -> unify rest
        (VarT s, _) -> if contains s y
                       then error ("Cyclic type: " ++ show (x,y))
                       else extendCtx s y $ unify (map (subs s y) rest)
        (_, VarT s) -> if contains s x
                       then error ("Cyclic type: " ++ show (y,x))
                       else extendCtx s x $ unify (map (subs s x) rest)
        (FunT t1 t2, FunT s1 s2) ->
                unify $ (t1, s1):(t2,s2):rest
        (VectorT t, VectorT s) -> unify $ (t, s):rest
        -- (RecV, RecV)  -> TODO: Tricky case, do later
        otherwise -> error ("Incompatible types: " ++ show (x,y))


subs :: Symbol -> Type -> (Type, Type) -> (Type, Type)
subs s ty (t1, t2) = (subs1 s ty t1, subs1 s ty t2)
    
subs1 ::  Symbol -> Type -> Type -> Type
subs1 s ty t@(VarT s2) = if s2 == s then ty else t
subs1 s ty (RecT lst) = RecT $ map (applySnd $ subs1 s ty) lst
subs1 s ty (VectorT t) = VectorT $ subs1 s ty t
subs1 s ty (FunT t1 t2) = FunT (subs1 s ty t1) (subs1 s ty t2)
subs1 _ _ t = t
