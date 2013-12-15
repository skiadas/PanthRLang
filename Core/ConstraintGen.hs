module ConstraintGen where

import Types
import MyState
import Env
import TypeCheckTypes

genConstraints :: TExpr -> TypingStMonad (TExpr)
genConstraints e =
    let (Typed ty ex) = e
    in case ex of
        (IntE  _) -> return e;
        (DblE  _) -> return e;
        (BoolE _) -> return e;
        (StrE  _) -> return e;
        (VectorE lst) -> do {
            t <- freeType;
            lstT <- fmap (map typ) $ sequenceStates (map genConstraints lst);
            addConstraintList $ (VectorT t, ty):(map ((,) t) lstT);
            return $ Typed (VectorT t) ex;
        }
        (ArithmE op e1 e2) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            te2 <- fmap typ $ genConstraints e2;
            addConstraintList [(t, ty), (t, te1), (t, te2)];
            if (op == OpDivide) then addConstraintList [(te1, DblT), (te2, DblT)]
                                else addConstraintList [(te1, NumT), (te2, NumT)];
            return $ Typed t ex;
        }
        (LogicalE _ e1 e2) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            te2 <- fmap typ $ genConstraints e2;
            addConstraintList [(t, BoolT), (te1, BoolT), (te2, BoolT)];
            return $ Typed t ex;
        }
        (NegateE e1) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            addConstraintList [(t, te1), (t, NumT), (te1, NumT)]; -- Todo: Figure out to add number constraint
            return $ Typed t ex;
        }
        (NotE e1) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            addConstraintList [(te1, BoolT), (t, BoolT)];
            return $ Typed t ex;
        }
        (IfE e_if e_then e_else) -> do {
            t <- freeType;
            te_if   <- fmap typ $ genConstraints e_if;
            te_then <- fmap typ $ genConstraints e_then;
            te_else <- fmap typ $ genConstraints e_else;
            addConstraintList [(te_if, BoolT), (t, te_then), (t, te_else)];
            return $ Typed t ex;
        }
        (VarE s) -> do {
            ctx <- fmap (ctx . getContext) $ getState;
            let t = (unMaybe ("Unbound identifier" ++ show s) $ locate ctx s)
            in addConstraintList [(t, ty)] >> (return $ Typed t ex);
        }
        (FunE (LambdaE s e1)) -> do {
            t <- freeType;
            st <- getState;
            let (Typed te1 _, st2) = runState (genConstraints e1)
                                              (liftCtx (extendCtx s t) st)
            in do {
                putState $ liftCtx (\_ -> getContext st) st2;
                addConstraintList [(FunT t te1, ty)];
                return $ Typed (FunT t te1) ex;
            }
        }
        (CallE e1 e2) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            te2 <- fmap typ $ genConstraints e2;
            addConstraintList [(t, ty), (te1, FunT te2 t)];
            return $ Typed t ex;
        }
        -- -- RecE still left
        -- -- FieldE still left
        otherwise -> error "Not implemented yet";
