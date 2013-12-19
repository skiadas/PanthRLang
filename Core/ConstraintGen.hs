module ConstraintGen where

import Syntax
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
            lstT <- sequenceStates (map genConstraints lst);
            addConstraintList $ (VectorT t, ty):(map (((,) t) . typ) lstT);
            return $ Typed (VectorT t) (VectorE lstT);
        }
        (ArithmE op e1 e2) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            te2 <- genConstraints e2;
            addConstraintList [(t, ty), (t, typ te1), (t, typ te2)];
            if (op == OpDivide) then addConstraintList [(typ te1, DblT), (typ te2, DblT)]
                                else addConstraintList [(typ te1, NumT), (typ te2, NumT)];
            return $ Typed t (ArithmE op te1 te2);
        }
        (LogicalE op e1 e2) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            te2 <- genConstraints e2;
            addConstraintList [(t, BoolT), (typ te1, BoolT), (typ te2, BoolT)];
            return $ Typed t (LogicalE op te1 te2);
        }
        (NegateE e1) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            addConstraintList [(t, typ te1), (t, NumT), (typ te1, NumT)]; -- Todo: Figure out to add number constraint
            return $ Typed t (NegateE te1);
        }
        (NotE e1) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            addConstraintList [(typ te1, BoolT), (t, BoolT)];
            return $ Typed t (NotE te1);
        }
        (IfE e_if e_then e_else) -> do {
            t <- freeType;
            te_if   <- genConstraints e_if;
            te_then <- genConstraints e_then;
            te_else <- genConstraints e_else;
            addConstraintList [(typ te_if, BoolT), (t, typ te_then), (t, typ te_else)];
            return $ Typed t (IfE te_if te_then te_else);
        }
        (VarE s) -> do {
            ctx <- fmap (ctx . getContext) $ getState;
            let t = (unMaybe ("Unbound identifier" ++ show s) $ locate ctx s)
            in addConstraintList [(t, ty)] >> (return $ Typed t ex);
        }
        (FunE (LambdaE s e1)) -> do {
            t <- freeType;
            st <- getState;
            let (te1, st2) = runState (genConstraints e1)
                                      (liftCtx (extendCtx s t) st)
            in do {
                putState $ liftCtx (\_ -> getContext st) st2;
                addConstraintList [(FunT t (typ te1), ty)];
                return $ Typed (FunT t (typ te1)) (FunE (LambdaE s te1));
            }
        }
        (CallE e1 e2) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            te2 <- genConstraints e2;
            addConstraintList [(t, ty), (typ te1, FunT (typ te2) t)];
            return $ Typed t (CallE te1 te2);
        }
        -- Need LetE
        -- RecE still left
        -- FieldE still left
        otherwise -> error "Not implemented yet";
