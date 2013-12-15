module TypeCheck where

import Parse  -- TODO: Remove later?
import MyState
import TypeCheckTypes
import ConstraintGen

typeCheck str = case parseExpr str of
        Left e -> error ("Error during parsing: " ++ show e)
        Right v -> typeCheckExpr v
        -- Right v -> (t2, con2) where
        --     (t, con) = typeCheckExpr v
        --     con2 = resolveTypes . reverse $ unify con
        --     t2 = fixTypes t con2

typeCheckExpr expr = runState (genConstraints expr) $ emptyState


-- Applies constraint unification.
-- Parameter is the remaining constraints
-- Return value is the resolved constraints
-- unify :: Constraints -> Context
-- unify [] = []
-- unify ((x,y):rest) =
--     if x == y
--     then unify rest
--     else case (x,y) of
--         (VarT s, _) -> if contains s y
--                        then error ("Cyclic type: " ++ show (x,y))
--                        else (s,y):(unify (map (subs s y) rest))
--         (_, VarT s) -> if contains s x
--                        then error ("Cyclic type: " ++ show (y,x))
--                        else (s,x):(unify (map (subs s x) rest))
--         (FunT t1 t2, FunT s1 s2) ->
--                 unify $ (t1, s1):(t2,s2):rest
--         (VectorT t, VectorT s) -> unify $ (t, s):rest
--         -- (RecV, RecV)  -> TODO: Tricky case, do later
--         otherwise -> error ("Incompatible types: " ++ show (x,y))
-- 
-- subs :: Symbol -> Type -> (Type, Type) -> (Type, Type)
-- subs s ty (t1, t2) = (subs1 s ty t1, subs1 s ty t2)
--     
-- subs1 ::  Symbol -> Type -> Type -> Type
-- subs1 s ty t@(VarT s2) = if s2 == s then ty else t
-- subs1 s ty (RecT lst) = RecT $ map (applySnd $ subs1 s ty) lst
-- subs1 s ty (VectorT t) = VectorT $ subs1 s ty t
-- subs1 s ty (FunT t1 t2) = FunT (subs1 s ty t1) (subs1 s ty t2)
-- subs1 _ _ t = t
-- 
-- -- resolves types from left to right by substituting into the remaining types.
-- -- requires reversing the list provided by unify
-- resolveTypes :: TypeContext -> TypeContext
-- resolveTypes [] = []
-- resolveTypes ((s, ty):rest) = (s, ty):resolveTypes (map (applySnd $ subs1 s ty) rest)
-- 
-- applySnd :: (b -> c) -> (a,b) -> (a,c)
-- applySnd f (a, b) = (a, f b)
-- 
-- fixTypes :: Typed Expr -> TypeContext -> Typed Expr
-- fixTypes te _ = te  -- FIXME for now till we move typed expressions into the parse process
-- -- fixTypes te = foldl fixType te
-- -- 
-- -- fixType :: Typed Expr -> (Symbol, Type) -> Typed Expr
-- -- fixType (t, expr) (s, ty) = (subs1 s ty t, fixExpr expr (s, ty))
-- -- 
-- 
