module TypeCheck where

import Utils
import Env
import Syntax
import Parse  -- TODO: Remove later?
import MyState
import TypeCheckTypes
import ConstraintGen
import Unify

typeCheck str = case parseExpr str of
        Left e -> error ("Error during parsing: " ++ show e)
        Right v -> (e2, ctx, ctx2) where
                        (e, TypingSt ctx con _) = typeCheckExpr v
                        ctx2 = unify con
                        e2 = fixTExpr ctx2 e
        -- Right v -> (t2, con2) where
        --     (t, con) = typeCheckExpr v
        --     con2 = resolveTypes . reverse $ unify con
        --     t2 = fixTypes t con2

typeCheckExpr expr = runState (genConstraints expr) $ emptyState


fixTExpr :: Context -> TExpr -> TExpr
fixTExpr (Cntx env) (Typed ty e) = Typed ty2 e2 where
                ty2 = substTypes env ty
                e2  = substInExpr (Cntx env) e

substInExpr :: Context -> Expr -> Expr
substInExpr (Cntx env) e = let fixTE = fixTExpr (Cntx env) in case e of
        (VectorE lst) -> VectorE (map fixTE lst)
        (RecE lst)    -> RecE (map (applySnd fixTE) lst)
        (ArithmE  op te1 te2) -> ArithmE  op (fixTE te1) (fixTE te2)
        (LogicalE op te1 te2) -> LogicalE op (fixTE te1) (fixTE te2)
        (CompareE op te1 te2) -> CompareE op (fixTE te1) (fixTE te2)
        (NegateE te) -> NegateE (fixTE te)
        (NotE te)    -> NotE (fixTE te)
        (IfE te1 te2 te3)     -> IfE (fixTE te1) (fixTE te2) (fixTE te3)
        (FunE (LambdaE s te)) -> FunE (LambdaE s (fixTE te))
        (CallE te1 te2)       -> CallE (fixTE te1) (fixTE te2)
        (FieldE te s)         -> FieldE (fixTE te) s
        (LetE (p, te1) te2)   -> (LetE (p, (fixTE te1)) (fixTE te2))
        otherwise -> e
