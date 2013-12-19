module Unify (unify, substTypes) where

import Utils
import Syntax
import Env
import TypeCheckTypes

-- Applies constraint unification.
-- Parameter is the remaining constraints
-- Return value is the resolved constraints
unify :: Constraints -> Context
unify lst = case unifyInternal (lst, []) of
        (cons, []) -> backSubstituteCtx cons
        (cons, unresolved) -> error ("Unresolved constraints: " ++ show (cons, unresolved))

-- Second parameter in input and output is the constraints that
-- for some reason need further processing/cannot be resolved
-- The method subsTwo is responsible for moving unresolved
-- constraints back to the first argument, to be reprocessed
unifyInternal :: (Constraints, Constraints) -> (Context, Constraints)
unifyInternal ([], unresolved) = (emptyContext, unresolved)
unifyInternal ((x,y):rest, unresolved) =
    if x == y
    then unifyInternal (rest, unresolved)
    else let remn = (rest, unresolved) in case (x, y) of
        (IntT, NumT) -> unifyInternal remn
        (DblT, NumT) -> unifyInternal remn
        (NumT, IntT) -> unifyInternal remn
        (NumT, DblT) -> unifyInternal remn
        (VarT _, NumT) -> unifyInternal (rest, (x,y):unresolved)
        (_, UndefT) -> unifyInternal remn
        (UndefT, _) -> unifyInternal remn
        (VarT s, _) -> if contains s y
                       then error ("Cyclic type: " ++ show (x,y))
                       else applyFst (extendCtx s y) $ unifyInternal (subsCtx s y remn)
        (_, VarT s) -> if contains s x
                       then error ("Cyclic type: " ++ show (y,x))
                       else applyFst (extendCtx s x) $ unifyInternal (subsCtx s x remn)
        (FunT t1 t2, FunT s1 s2) ->
                unifyInternal $ ((t1, s1):(t2,s2):rest, unresolved)
        (VectorT t, VectorT s) -> unifyInternal $ ((t, s):rest, unresolved)
        -- (RecV, RecV)  -> TODO: Tricky case, do later
        otherwise -> error ("Incompatible types: " ++ show (x,y))


subsCtx :: Symbol -> Type -> (Constraints, Constraints) -> (Constraints, Constraints)
subsCtx s ty (cons1a, cons1b) = ((map (subs s ty) cons1a) ++ cons2a, cons2b)
                                where (cons2a, cons2b) = subsConstr s ty cons1b

subsConstr :: Symbol -> Type -> Constraints -> (Constraints, Constraints)
subsConstr s ty [] = ([], [])
subsConstr s ty ((t1, t2):rest)
    | (contains s t1 || contains s t2) = (subbed:rest2, remn)
    | otherwise                        = (rest2, subbed:remn)
                where subbed = (subs s ty (t1, t2))
                      (rest2, remn) = subsConstr s ty rest

subs :: Symbol -> Type -> (Type, Type) -> (Type, Type)
subs s ty (t1, t2) = (subs1 s ty t1, subs1 s ty t2)
    
subs1 ::  Symbol -> Type -> Type -> Type
subs1 s ty t@(VarT s2) = if s2 == s then ty else t
subs1 s ty (RecT lst) = RecT $ map (applySnd $ subs1 s ty) lst
subs1 s ty (VectorT t) = VectorT $ subs1 s ty t
subs1 s ty (FunT t1 t2) = FunT (subs1 s ty t1) (subs1 s ty t2)
subs1 _ _ t = t

backSubstituteCtx :: Context -> Context
backSubstituteCtx (Cntx env) = Cntx (backSubstitute env)

backSubstitute :: Env Type -> Env Type
backSubstitute [] = []
backSubstitute ((s, ty):rest) = (s, ty2):rest2
                                where rest2 = backSubstitute rest
                                      ty2 = substTypes rest2 ty

-- Substitutes undetermined types in Type as specified in the environment
substTypes :: Env Type -> Type -> Type
substTypes env ty = case ty of
    (RecT lst) -> RecT (map (applySnd (substTypes env)) lst)
    (VectorT t) -> VectorT (substTypes env t)
    (FunT t1 t2) -> FunT (substTypes env t1) (substTypes env t2)
    (VarT s) -> case locate env s of 
                    Nothing -> ty
                    Just t -> t
    otherwise -> ty
