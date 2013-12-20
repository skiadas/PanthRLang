module ConstraintGen where

import Syntax
import MaybeE
import MyState
import Control.Applicative
import Env
import TypeCheckTypes

genConstraintsS :: SrcExpr -> TypingStMonad (SrcExpr)
genConstraintsS (Error e) = return (Error e)
genConstraintsS (Ok v)    = genConstraints v

genConstraints :: Expr Info -> TypingStMonad (SrcExpr)
genConstraints e =
    let inf@(Info (pos, ty)) = info e
    in case e of
        (IntE  _ _) -> return (fmap (setTyp IntT)  $ pure e);
        (DblE  _ _) -> return (fmap (setTyp DblT)  $ pure e);
        (BoolE _ _) -> return (fmap (setTyp BoolT) $ pure e);
        (StrE  _ _) -> return (fmap (setTyp StrT)  $ pure e);
        (VectorE _ lst) -> do {
            t <- freeType;
            lstT <- fmap sequence $ sequenceStates (map genConstraints lst);
            addConstraintList $ (VectorT t, ty):(combineTyps [t] $ getTyps lstT);
            return $ ok (VectorE (setTyp (VectorT t) inf)) <*> lstT;
        }
        (ArithmE _ op e1 e2) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            te2 <- genConstraints e2;
            let t1 = getTyp te1
                t2 = getTyp te2
                supTyp = if op == OpDivide then DblT else NumT
            in  addConstraintList $ (t, ty):combineTyps [t, supTyp] (t1++t2);
            return $ ok (ArithmE (setTyp t inf) op) <*> te1 <*> te2;
        }
        (LogicalE _ op e1 e2) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            te2 <- genConstraints e2;
            addConstraintList $ combineTyps (t : (getTyp te1) ++ (getTyp te2)) [BoolT];
            return $ ok (LogicalE (setTyp t inf) op) <*> te1 <*> te2;
        }
        (NegateE _ e1) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            let t1 = getTyp te1
            in addConstraintList $ (t, NumT):combineTyps [t, NumT] t1; -- Todo: Figure out to add number constraint
            return $ ok (NegateE (setTyp t inf)) <*> te1;
        }
        (NotE _ e1) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            addConstraintList $ combineTyps (t:(getTyp te1)) [BoolT];
            return $ ok (NotE (setTyp t inf)) <*> te1;
        }
        (IfE _ e_if e_then e_else) -> do {
            t <- freeType;
            te_if   <- genConstraints e_if;
            te_then <- genConstraints e_then;
            te_else <- genConstraints e_else;
            addConstraintList $ combineTyps (getTyp te_if) [BoolT]
                             ++ combineTyps [t] (getTyp te_then ++ getTyp te_else);
            return $ ok (IfE (setTyp t inf)) <*> te_if <*> te_then <*> te_else;
        }
        (VarE _ s) -> do {
            ctx <- fmap (ctx . getContext) $ getState;
            let te = tryE (locate ctx s)
                     (err pos (["Unbound identifier: " ++ show s]))
            in addConstraintList (combineTyps (unmaybeTyp te) [typ inf]) 
                >> (return $ ok (\t -> VarE (setTyp t inf) s) <*> te);
        }
        (FunE (LambdaE _ s e1)) -> do {
            t <- freeType;
            st <- getState;
            let (te1, st2) = runState (genConstraints e1)
                                      (liftCtx (extendCtx s t) st)
                ft = pure ((FunT t) . typ) <*> te1      --- MaybeE Type
            in do {
                putState $ liftCtx (\_ -> getContext st) st2;
                addConstraintList $ combineTyps (unmaybeTyp ft) [ty];
                return $ ok (\te -> (FunE (LambdaE (setTyp (FunT t (typ $ info te)) inf) s te))) <*> te1;
            }
        }
        (CallE _ e1 e2) -> do {
            t <- freeType;
            te1 <- genConstraints e1;
            te2 <- genConstraints e2;
            addConstraintList ((t, ty):combineTyps (getTyp te1) (getTyp te2 >>= (\te -> [FunT te t])));
            return $ ok (CallE (setTyp t inf)) <*> te1 <*> te2;
        }
        -- Need LetE
        -- RecE still left
        -- FieldE still left
        otherwise -> return $ err Nothing ["Not implemented yet"];


getTyp :: SrcExpr -> [Type]
getTyp (Error e) = []
getTyp (Ok v)    = [typ $ info $ v]

unmaybeTyp :: MaybeE Type -> [Type]
unmaybeTyp (Error e) = []
unmaybeTyp (Ok t)    = [t]

getTyps :: MaybeE [Expr Info] -> [Type]
getTyps (Error e) = []
getTyps (Ok lst)  = map (typ . info) lst

combineTyps :: [Type] -> [Type] -> [(Type, Type)]
combineTyps lst1 lst2 = [(t1, t2) | t1 <- lst1, t2 <- lst2]

tryE :: Maybe a -> MaybeE a -> MaybeE a
tryE Nothing a  = a
tryE (Just v) _ = ok v
