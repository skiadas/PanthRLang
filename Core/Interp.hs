-- Interpretor for the Core language

module Interp where
    
import Types
import Env
import Store
import Parse(parseExpr)
import BuiltIns(builtInEnv)
import MyState
import PatternMatch

data IntrSt = IntrSt { env :: Env Value, st :: Store Value }
type IntrState = MyState IntrSt

-- Replace emptyEnv with builtInEnv or find way to enter that in
emptyState = IntrSt emptyEnv emptyStore

getEnv :: IntrState (Env Value)
getEnv = MyState $ \st -> (env st, st)

setEnv :: Env Value -> IntrState ()
setEnv env = MyState $ \(IntrSt _ store) -> ((), IntrSt env store)

-- Given an environment and a "state action", returns a
-- "state action" that performs the previous state using
-- the provided environment temporarily.
-- Essentially, implements the lexical scope mechanic
runWithTempEnv :: Env Value -> IntrState a -> IntrState a
runWithTempEnv env b = do {
    envOrig <- getEnv;
    setEnv env; v <- b; setEnv envOrig;
    return v;
}

eval :: String -> Value
eval s = case parseExpr s of
    Left e -> error ("Error during parsing: " ++ show e)
    Right v -> interp v

interp :: TExpr -> Value
interp e = getValue $ runState (interpS e) emptyState

interpS :: TExpr -> IntrState Value
interpS (Typed _ (IntE  i)) = return (IntV  i)
interpS (Typed _ (DblE  d)) = return (DblV  d)
interpS (Typed _ (BoolE b)) = return (BoolV b)
interpS (Typed _ (StrE  s)) = return (StrV  s)
interpS (Typed _ (VectorE lst)) = fmap VectorV $ sequenceStates (map interpS lst)
interpS (Typed _ (RecE lst)) = fmap RecV $ fmap (\(s,t) -> zip s t)
                               $ fmap ((,) ids)
                                      (sequenceStates $ map interpS exps)
                                                where (ids, exps) = unzip lst
interpS (Typed _ (FieldE e s)) = do { -- Should be able to make this shorter
    v <- (interpS e);
    case v of
        (RecV obj) -> return $ unMaybe ("Field not in record: " ++ show s) (locate obj s);
        _ -> fail "Attempting to access field of non-record.";
}
interpS (Typed _ (ArithmE op e1 e2)) = do {
    v1 <- interpS e1;
    v2 <- interpS e2;
    return $ applyArithmOp op v1 v2;
}
interpS (Typed _ (LogicalE OpAnd e1 e2)) = do {
        v1 <- interpS e1;
        if (valToBool v1)
        then interpS e2  -- Should be doing ensureBool here?
        else return v1;
}
interpS (Typed _ (LogicalE OpOr e1 e2)) = do {
        v1 <- interpS e1;
        if valToBool v1
        then return v1
        else interpS e2;
}
interpS (Typed _ (NegateE e)) = (fmap negateV) (interpS e)
interpS (Typed _ (NotE e)) = (fmap notV) (interpS e)
interpS (Typed _ (IfE e_if e_then e_else)) = do {
    v <- interpS e_if;
    interpS $ if (valToBool v) then e_then else e_else;
}
interpS (Typed _ (VarE s)) = fmap ((unMaybe ("Unbound identifier: " ++ show s)) . (`locate` s)) getEnv
interpS (Typed _ (FunE e)) = fmap (`ClosV` e) getEnv
interpS (Typed _ (CallE e1 e2)) = do {
    v1 <- interpS e1;
    case v1 of 
        (ClosV env1 (LambdaE s body)) ->  do {
            v2 <- interpS e2;
            runWithTempEnv (extend env1 s v2) (interpS body)
        }
        (ClosV _ (BuiltInE f)) -> do {
            v2 <- interpS e2;
            return (f v2);
        }
        _ -> error "Attempting to call non-function"
}
interpS (Typed _ (LetE (p, e) bodyE)) = do {
    envOrig <- getEnv;
    v <- interpS e;
    runWithTempEnv (extendBinding envOrig p v) (interpS bodyE);
}


applyArithmOp op v1 v2 =
    case (op, uniformizeNums(v1, v2)) of
            (OpDivide, (IntV i1, IntV i2)) -> error "Attempt to use '/' on integers"
            (_, (IntV i1, IntV i2)) -> IntV ((intFunFromArithmOp op) i1 i2)
            (_, (DblV i1, DblV i2)) -> DblV ((dblFunFromArithmOp op) i1 i2)
            _ -> error "Attempting to perform arithmetic on non-numbers"

uniformizeNums(IntV i, DblV j) = (DblV (fromInteger i), DblV j)
uniformizeNums(DblV i, IntV j) = (DblV i, DblV (fromInteger j))
uniformizeNums v = v

negateV :: Value -> Value
negateV (IntV i) = IntV (-i)
negateV (DblV d) = DblV (-d)
negateV        _ = error "'negate' used on non-number";

notV :: Value -> Value
notV (BoolV b) = BoolV (not b)
notV         _ = error "'not' used on non-boolean"
