module TypeCheck where

import Parse  -- TODO: Remove later?
import MyState
import TypeCheckTypes
import ConstraintGen
import Unify

typeCheck str = case parseExpr str of
        Left e -> error ("Error during parsing: " ++ show e)
        Right v -> (e, ctx, ctx2) where
                        (e, TypingSt ctx con _) = typeCheckExpr v
                        ctx2 = unify con
        -- Right v -> (t2, con2) where
        --     (t, con) = typeCheckExpr v
        --     con2 = resolveTypes . reverse $ unify con
        --     t2 = fixTypes t con2

typeCheckExpr expr = runState (genConstraints expr) $ emptyState


-- 
-- 
-- -- resolves types from left to right by substituting into the remaining types.
-- -- requires reversing the list provided by unify
-- resolveTypes :: TypeContext -> TypeContext
-- resolveTypes [] = []
-- resolveTypes ((s, ty):rest) = (s, ty):resolveTypes (map (applySnd $ subs1 s ty) rest)
-- 
-- 
-- fixTypes :: Typed Expr -> TypeContext -> Typed Expr
-- fixTypes te _ = te  -- FIXME for now till we move typed expressions into the parse process
-- -- fixTypes te = foldl fixType te
-- -- 
-- -- fixType :: Typed Expr -> (Symbol, Type) -> Typed Expr
-- -- fixType (t, expr) (s, ty) = (subs1 s ty t, fixExpr expr (s, ty))
-- -- 
-- 
