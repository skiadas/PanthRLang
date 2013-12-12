-- Interpretor for the Core language

module Interp where
    
import Types
import Env
import Store
import Parse(parseExpr)
import BuiltIns(builtInEnv)

interp :: Expr -> Env Value -> Store Value -> (Value, Store Value)
interp (IntE i) _ st  = (IntV  i, st)
interp (DblE d) _ st  = (DblV  d, st)
interp (BoolE b) _ st = (BoolV b, st)
interp (StrE s) _ st  = (StrV  s, st)
interp (VectorE lst) env st = (VectorV lstV, st2)
                where (lstV, st2) = listEval (lst, st)
                      listEval ([], st) = ([], st)
                      listEval (e:rest, st) = let (v, st1) = interp e env st
                                                  (restv, stend) = listEval (rest, st1)
                                              in  (v:restv, stend)
interp (ArithmE op e1 e2) env st = (applyArithmOp op v1 v2, st2)
                where (v1, st1) = interp e1 env st
                      (v2, st2) = interp e2 env st1
interp (CompareE op e1 e2) env st = (applyCompOp op v1 v2, st2)
                where (v1, st1) = interp e1 env st
                      (v2, st2) = interp e2 env st1
interp (LogicalE OpAnd e1 e2) env st =
        let (v1, st1) = interp e1 env st
        in if valToBool v1
           then let (v2, st2) = interp e2 env st1
                in (ensureBool v2, st2)
           else (v1, st1)
interp (LogicalE OpOr e1 e2) env st = 
        let (v1, st1) = interp e1 env st
        in if valToBool v1
           then (v1, st1)
           else let (v2, st2) = interp e2 env st1
                in (ensureBool v2, st2)
interp (NegateE e) env st = (retV, st1)
                    where (v1, st1) = interp e env st
                          retV = case v1 of
                              IntV i -> IntV (-i)
                              DblV d -> DblV (-d)
                              _ -> error "'negate' used on non-number"
interp (NotE e) env st = (retV, st1)
                    where (v1, st1) = interp e env st
                          retV = case v1 of
                              BoolV b -> BoolV (not b)
                              _ -> error "'not' used on non-boolean"
interp (IfE e_if e_then e_else) env st =
        let (v1, st1) = interp e_if env st
            which_e = if (valToBool v1) then e_then else e_else
        in interp which_e env st1
interp (VarE s) env st = case locate env s of
            Just v -> (v, st)
            Nothing -> error ("Unbound identifier: " ++ show s)
interp (FunE e) env st = (ClosV env e, st)
interp (CallE e1 e2) env st = 
    let (v1, st1) = interp e1 env st
    in case v1 of
        (ClosV env1 (LambdaE s body)) ->
            let (v2, st2) = interp e2 env st1
            in interp body (extend env s v2) st2
        (ClosV _ (BuiltInE f)) ->
            let (v2, st2) = interp e2 env st1
            in (f v2, st2)
        _ -> error "Attempting to call non-function"


applyArithmOp op v1 v2 =
    case (op, uniformizeNums(v1, v2)) of
            (OpDivide, (IntV i1, IntV i2)) -> error "Attempt to use '/' on integers"
            (_, (IntV i1, IntV i2)) -> IntV ((intFunFromArithmOp op) i1 i2)
            (_, (DblV i1, DblV i2)) -> DblV ((dblFunFromArithmOp op) i1 i2)
            _ -> error "Attempting to perform arithmetic on non-numbers"

applyCompOp op v1 v2 =
    case uniformizeNums(v1, v2) of
            (IntV i1, IntV i2) -> BoolV (applyOp op i1 i2)
            (DblV i1, DblV i2) -> BoolV (applyOp op i1 i2)
            _ -> error "Attempting to perform inequalities on non-numbers"

uniformizeNums(IntV i, DblV j) = (DblV (fromInteger i), DblV j)
uniformizeNums(DblV i, IntV j) = (DblV i, DblV (fromInteger j))
uniformizeNums v = v

eval :: String -> Value
eval s = case parseExpr s of
    Left e -> error ("Error during parsing: " ++ show e)
    Right v -> v1 where (v1, st) = interp v builtInEnv emptyStore

