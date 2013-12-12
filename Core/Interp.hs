-- Interpretor for the Core language

module Interp where
    
import Types
import Env
import Store
import Parse(parseExpr)
import BuiltIns(builtInEnv)
import MyState

data IntrSt = IntrSt { env :: Env Value, st :: Store Value }
type IntrState = MyState IntrSt

-- Replace emptyEnv with builtInEnv or find way to enter that in
emptyState = IntrSt emptyEnv emptyStore

getValue :: (a, b) -> a
getValue (a, b) = a

getEnv :: IntrState (Env Value)
getEnv = MyState $ \st -> (env st, st)

-- Given an environment and a "state action", returns a
-- "state action" that performs the previous state using
-- the provided environment temporarily.
-- Essentially, implements the lexical scope mechanic
runWithTempEnv :: (Env Value) -> IntrState a -> IntrState a
runWithTempEnv env b = MyState $ (\st ->
    let (IntrSt envOrig store) = st
        (v, IntrSt _ store1) = runState b $ (IntrSt env store)
    in (v, IntrSt envOrig store1))

eval :: String -> Value
eval s = case parseExpr s of
    Left e -> error ("Error during parsing: " ++ show e)
    Right v -> interp v

interp :: Expr -> Value
interp e = getValue $ runState (interp e) emptyState

interp :: Expr -> IntrState Value
interp (IntE  i) = return (IntV  i)
interp (DblE  d) = return (DblV  d)
interp (BoolE b) = return (BoolV b)
interp (StrE  s) = return (StrV  s)
interp (VectorE lst) = fmap VectorV $ sequenceStates (map interp lst)
interp (ArithmE op e1 e2) = do {
    v1 <- interp e1;
    v2 <- interp e2;
    return $ applyArithmOp op v1 v2;
}
interp (LogicalE OpAnd e1 e2) = do {
        v1 <- interp e1;
        if (valToBool v1)
        then interp e2  -- Should be doing ensureBool here?
        else return v1;
}
interp (LogicalE OpOr e1 e2) = do {
        v1 <- interp e1;
        if valToBool v1
        then return v1
        else interp e2;
}
interp (NegateE e) = (fmap negateV) (interp e)
interp (NotE e) = (fmap notV) (interp e)
interp (IfE e_if e_then e_else) = do {
    v <- interp e_if;
    interp $ if (valToBool v) then e_then else e_else;
}
interp (VarE s) = fmap ((unMaybe ("Unbound identifier: " ++ show s)) . (`locate` s)) getEnv
interp (FunE e) = fmap (`ClosV` e) getEnv
interp (CallE e1 e2) = do {
    v1 <- interp e1;
    case v1 of 
        (ClosV env1 (LambdaE s body)) ->  do {
            v2 <- interp e2;
            runWithTempEnv (extend env1 s v2) (interp body)
        }
        (ClosV _ (BuiltInE f)) -> do {
            v2 <- interp e2;
            return (f v2);
        }
        _ -> error "Attempting to call non-function"
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

unMaybe :: String -> Maybe a -> a
unMaybe s (Just v) = v
unMaybe s Nothing = error s
