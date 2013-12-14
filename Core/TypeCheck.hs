module TypeCheck where

import Types
import Env
import Parse  -- TODO: Remove later?
import MyState

type Context = Env Type
data Typed a = Typed { typ :: Type, val :: a } deriving (Show)
type NamesGen = [Symbol]
type Constraints = [(Type, Type)]
data ConstrSt = ConstrSt { ctx :: Context, con :: Constraints, gen :: NamesGen } deriving (Show)
type ConstrState = MyState ConstrSt

generate :: NamesGen -> (Symbol, NamesGen)
generate (x:xs) = (x, xs)

initGen = map (\n -> toSymbol $ '_':show n) [1..]

freeType :: ConstrState Type
freeType = MyState (\(ConstrSt cx cont g) -> 
                let (v, g2) = generate g
                in (VarT v, (ConstrSt cx cont g2)))

addConstraintList :: Constraints -> ConstrState ()
addConstraintList lst = MyState (\(ConstrSt cx cont g) -> ((), ConstrSt cx (lst ++ cont) g))


typeCheck str = case parseExpr str of
        Left e -> error ("Error during parsing: " ++ show e)
        Right v -> (\(t, con) -> (t, con, unify con)) $ typeCheckExpr v

typeCheckExpr expr = (te, con st)
    where (te, st) = runState (genConstraints expr) $ ConstrSt [] [] initGen

genConstraints :: Expr -> ConstrState (Typed Expr)
-- genConstraints (IntE i) = return $ (Typed IntT (IntE i))
genConstraints e =
    case e of
        (IntE  _) -> return $ Typed IntT  e;
        (DblE  _) -> return $ Typed DblT  e;
        (BoolE _) -> return $ Typed BoolT e;
        (StrE  _) -> return $ Typed StrT  e;
        (VectorE lst) -> do {
            t <- freeType;
            lstT <- fmap (map typ) $ sequenceStates (map genConstraints lst);
            addConstraintList $ (map ((,) t) lstT);
            return $ Typed (VectorT t) e;
        }
        -- RecE still left
        -- FieldE still left
        (ArithmE op e1 e2) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            te2 <- fmap typ $ genConstraints e2;
            addConstraintList [(t, te1), (t, te2)];
            if (op == OpDivide) then addConstraintList [(te1, DblT), (te2, DblT)]
                                else return ();
            return $ Typed t e;
        }
        (LogicalE _ e1 e2) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            te2 <- fmap typ $ genConstraints e2;
            addConstraintList [(t, BoolT), (te1, BoolT), (te2, BoolT)];
            return $ Typed t e;
        }
        (NegateE e1) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            addConstraintList [(t, te1)]; -- Todo: Figure out to add number constraint
            return $ Typed t e;
        }
        (NotE e1) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            addConstraintList [(te1, BoolT), (t, BoolT)];
            return $ Typed t e;
        }
        (IfE e_if e_then e_else) -> do {
            t <- freeType;
            te_if   <- fmap typ $ genConstraints e_if;
            te_then <- fmap typ $ genConstraints e_then;
            te_else <- fmap typ $ genConstraints e_else;
            addConstraintList [(te_if, BoolT), (t, te_then), (t, te_else)];
            return $ Typed t e;
        }
        (VarE s) -> do {
            ctx <- fmap ctx $ getState;
            let t = (unMaybe ("Unbound identifier" ++ show s) $ locate ctx s)
            in return $ Typed t e;
        }
        (FunE (LambdaE s e1)) -> do {
            t <- freeType;
            (ConstrSt ctx constr gen) <- getState;
            te1 <- fmap typ $ let tmpCtx = extend ctx s t
                              in putState (ConstrSt tmpCtx constr gen)
                                 >> genConstraints e1;
            (ConstrSt _ constr1 gen1) <- getState;
            putState (ConstrSt ctx constr1 gen1);
            return $ Typed (FunT t te1) e;
        }
        (CallE e1 e2) -> do {
            t <- freeType;
            te1 <- fmap typ $ genConstraints e1;
            te2 <- fmap typ $ genConstraints e2;
            addConstraintList [(te1, FunT te2 t)];
            return $ Typed t e;
        }
        otherwise -> error "Not implemented yet";

-- Applies constraint unification.
-- Parameter is remaining constraints
-- Return value is the resolved constraints
unify :: Constraints -> Constraints
unify [] = []
unify ((x,y):rest) =
    if x == y
    then unify rest
    else case (x,y) of
        (VarT s, _) -> if contains s y
                       then error ("Cyclic type: " ++ show (x,y))
                       else (x,y):(unify (map (subs s y) rest))
        (_, VarT s) -> if contains s x
                       then error ("Cyclic type: " ++ show (y,x))
                       else (y,x):(unify (map (subs s x) rest))
        (FunT t1 t2, FunT s1 s2) ->
                unify $ (t1, s1):(t2,s2):rest
        (VectorT t, VectorT s) -> unify $ (t, s):rest
        -- (RecV, RecV)  -> TODO: Tricky case, do later
        otherwise -> error ("Incompatible types: " ++ show (x,y))

subs :: Symbol -> Type -> (Type, Type) -> (Type, Type)
subs s ty (t1, t2) = (subs1 s ty t1, subs1 s ty t2)
    
subs1 ::  Symbol -> Type -> Type -> Type
subs1 s ty t@(VarT s2) = if s2 == s then ty else t
subs1 s ty (RecT lst) = RecT $ zip syms (map (subs1 s ty) typs)
                        where (syms, typs) = unzip lst
subs1 s ty (VectorT t) = VectorT $ subs1 s ty t
subs1 s ty (FunT t1 t2) = FunT (subs1 s ty t1) (subs1 s ty t2)
subs1 _ _ t = t

