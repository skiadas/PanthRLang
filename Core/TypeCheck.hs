module TypeCheck where

import Utils
import Env
import Syntax
import Parse  -- TODO: Remove later?
import MyState
import TypeCheckTypes
import ConstraintGen
import Unify
import MaybeE

typeCheck :: String -> (SrcExpr, TypingState)
typeCheck = fixTypes . (applySnd unifyState) . typeCheckExpr . parseExpr


unifyState :: TypingState -> TypingState
unifyState (TypingSt ctx cons gen) = TypingSt ctx2 [] gen
                        where ctx2 = unify cons


typeCheckExpr :: SrcExpr -> (SrcExpr, TypingState)
typeCheckExpr expr = runState (genConstraintsS expr) $ emptyState

-- Uses the context information to substitute any type variables in the expr

fixTypes :: (SrcExpr, TypingState) -> (SrcExpr, TypingState)
fixTypes (se, st) = (pure (substInExpr (getContext st)) <*> se, st)

fixType :: Context -> Type -> Type
fixType (Cntx env) = substTypes env

fixInfo :: Context -> Info -> Info
fixInfo ctx inf = setTyp (fixType ctx (typ inf)) inf

substInExpr :: Context -> Expr Info -> Expr Info
substInExpr ctx e =
    let inf2 = fixInfo ctx (info e)
        fixExpr = substInExpr ctx
    in case e of
        (VectorE _ lst)         -> VectorE  inf2 (map fixExpr lst)
        (RecE    _ lst)         -> RecE     inf2 (map (applySnd fixExpr) lst)
        (ArithmE  _ op te1 te2) -> ArithmE  inf2 op (fixExpr te1) (fixExpr te2)
        (LogicalE _ op te1 te2) -> LogicalE inf2 op (fixExpr te1) (fixExpr te2)
        (CompareE _ op te1 te2) -> CompareE inf2 op (fixExpr te1) (fixExpr te2)
        (NegateE _ te)          -> NegateE  inf2 (fixExpr te)
        (NotE    _ te)          -> NotE     inf2 (fixExpr te)
        (VarE    _ s )          -> VarE     inf2 s
        (IfE _ te1 te2 te3)     -> IfE      inf2 (fixExpr te1) (fixExpr te2) (fixExpr te3)
        (FunE (LambdaE _ s te)) -> FunE (LambdaE inf2 s (fixExpr te))
        (CallE _ te1 te2)       -> CallE    inf2 (fixExpr te1) (fixExpr te2)
        (FieldE _ te s)         -> FieldE   inf2 (fixExpr te) s
        (LetE _ (p, te1) te2)   -> LetE     inf2 (p, (fixExpr te1)) (fixExpr te2)
        otherwise -> e
