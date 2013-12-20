module Syntax.Expr where

import Utils
import MaybeE
import Syntax.Type
import Syntax.Symbol
import Syntax.Function
import Syntax.Pattern(Pattern)
import Syntax.Value(Value)
import Syntax.Info

type SrcExpr = MExpr Info  -- Expressions with information on their source location

type MExpr a = MaybeE (Expr a)
-- The class a is used to carry information specific to each expression (type, source link etc)
data Expr a = IntE a Integer | DblE a Double | BoolE a Bool | StrE a String
            | VectorE a [Expr a]
            | ArithmE a ArithmOp (Expr a) (Expr a)
            | LogicalE a LogicOp (Expr a) (Expr a)
            | CompareE a CompareOp (Expr a) (Expr a) -- Binary Ops
            | NegateE a (Expr a) | NotE a (Expr a) -- Unary Ops
            | IfE a (Expr a) (Expr a) (Expr a)   -- if-then-else
            | FunE (Function Expr (Value Expr) a)
            | VarE a Symbol
            | CallE a (Expr a) (Expr a)
            | RecE a [(Symbol, (Expr a))] -- Records. Order does not matter. Doubling as tuples
            | FieldE a (Expr a) Symbol    -- Record field access.
            | LetE a (Pattern a, (Expr a)) (Expr a)   -- Acts as let*
          deriving (Show)

-- instance Show (Expr a) where
--     show (IntE _ e)            = show e
--     show (DblE _ e)            = show e
--     show (BoolE _ b)           = show b
--     show (StrE _ s)            = show s
--     show (VectorE _ lst)       = show lst
--     show (ArithmE _ op e1 e2)  = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
--     show (LogicalE _ op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
--     show (CompareE _ op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
--     show (NegateE _ e1)        = "(-" ++ show e1 ++ ")"
--     show (NotE _ e1)           = "(not " ++ show e1 ++ ")"
--     show (IfE _ e1 e2 e3)      = "if " ++ show e1 ++ " then " ++ show e2
--                                                 ++ " else " ++ show e3
--     show (FunE f)              = show f
--     show (VarE a s)            = show s
--     show (CallE _ e1 e2)       = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
--     show (RecE _ lst)          = "rec"   -- FIXME
--     show (FieldE _ e s)        = "field"  -- FIXME
--     show (LetE _ (p, e1) e2)   = "let " ++ show p ++ "=" ++ show e1 ++ " in " ++ show e2

instance Functor Expr where
    fmap f ex = case ex of
        (IntE a e)            -> (IntE (f a) e)
        (DblE a e)            -> (DblE (f a) e)
        (BoolE a b)           -> (BoolE (f a) b)
        (StrE a s)            -> (StrE (f a) s)
        (VectorE a lst)       -> (VectorE (f a) (map (fmap f) lst))
        (ArithmE a op e1 e2)  -> (ArithmE (f a) op (fmap f e1) (fmap f e2))
        (LogicalE a op e1 e2) -> (LogicalE (f a) op (fmap f e1) (fmap f e2))
        (CompareE a op e1 e2) -> (CompareE (f a) op (fmap f e1) (fmap f e2))
        (NegateE a e1)        -> (NegateE (f a) (fmap f e1))
        (NotE a e1)           -> (NotE (f a) (fmap f e1))
        (IfE a e1 e2 e3)      -> (IfE (f a) (fmap f e1) (fmap f e2) (fmap f e3))
        (FunE fun)            -> (FunE (fmap f fun))
        (VarE a s)            -> (VarE (f a) s)
        (CallE a e1 e2)       -> (CallE (f a) (fmap f e1) (fmap f e2))
        (RecE a lst)          -> (RecE (f a) (map (applySnd (fmap f)) lst))
        (FieldE a e s)        -> (FieldE (f a) (fmap f e) s)
        (LetE a (p, e1) e2)   -> (LetE (f a) ((fmap f p), (fmap f e1)) (fmap f e2))


-- Binary operators
data ArithmOp = OpPlus | OpMinus | OpMult | OpDivide deriving (Eq)
data LogicOp = OpAnd | OpOr deriving (Eq)
data CompareOp = OpGreater | OpLess | OpGeq | OpLeq | OpEq | OpNeq deriving (Eq)

instance Show ArithmOp where
    show OpPlus   = "+"
    show OpMinus  = "-"
    show OpMult   = "*"
    show OpDivide = "/"

instance Show LogicOp where
    show OpAnd = " and "
    show OpOr  = " or "

instance Show CompareOp where
    show OpGreater = ">"
    show OpLess    = "<"
    show OpGeq     = ">="
    show OpLeq     = "<="
    show OpEq      = "=="
    show OpNeq     = "/="

-- Extracts the info from the expression
instance Infoable Expr where
    info (IntE a _)         = a
    info (DblE a _)         = a
    info (BoolE a _)        = a
    info (StrE a _)         = a
    info (VectorE a _)      = a
    info (ArithmE a _ _ _)  = a
    info (LogicalE a _ _ _) = a
    info (CompareE a _ _ _) = a
    info (NegateE a _)      = a
    info (NotE a _)         = a
    info (IfE a _ _ _)      = a
    info (FunE f)           = info f
    info (VarE a _)         = a
    info (CallE a _ _)      = a
    info (RecE a _)         = a
    info (FieldE a _ _)     = a
    info (LetE a _ _)       = a


instance Typable a => Typable (Expr a) where
    typ      = typ . info
    setTyp t = fmap (setTyp t)

instance Sourceable a => Sourceable (Expr a) where
    src = src . info
    setSrc s = fmap (setSrc s)

--
-- Constructors
--
makeVector    = VectorE
makeIf        = IfE
makeBool      = BoolE
makeInt ty    = (IntE ty) . toInteger
makeDouble    = DblE
makeString    = StrE StrT
makeVar s ty  = (VarE ty) (makeSymbol s)
makeCompareOp op ty = CompareE ty op
makeArithmOp  op ty = ArithmE  ty op
makeLogicalOp op ty = LogicalE ty op
makeNot       = NotE
makeNegate    = NegateE
-- 
makeLambda :: [String] -> Expr a -> Expr a
makeLambda [] body = body
makeLambda [s] body = FunE $ LambdaE (info body) (makeSymbol s) body
makeLambda (s:rest) body = makeLambda [s] (makeLambda rest body)
-- 
makeField :: Expr a -> String -> Expr a
makeField obj s = FieldE (info obj) obj (makeSymbol s)

-- makeRecord :: Sourceable a => a -> [(String, Expr a)] -> MaybeE (Expr a)
-- makeRecord a lst = pure (RecE a) <*> (validateFieldNames $ map (applyFst makeSymbol) lst)

makeRecord :: a -> [(String, Expr a)] -> Expr a
makeRecord a lst = RecE a $ map (applyFst makeSymbol) lst

makeTuple :: a -> [Expr a] -> Expr a
makeTuple a lst = RecE a $ zip (map (makeSymbol . show) [1..(length lst)]) lst
-- 
makeFieldAccess :: Expr a -> [String] -> Expr a
makeFieldAccess = foldl (\e s -> FieldE (info e) e (makeSymbol s))

-- "makeCall f argsList"  creates a curried application of f on each arg in order
makeCall :: Expr a -> [Expr a] -> Expr a
makeCall = foldl (\f arg -> CallE (info f) f arg)
-- 
makeLet :: [(Pattern a, Expr a)] -> Expr a -> Expr a
makeLet bs ex = foldr (\b e -> LetE (info ex) b e) ex bs




validate :: Sourceable a => (a -> Bool) -> [String] -> a -> MaybeE a
validate f errMes a = if (f a) then ok a else err (src a) errMes

-- Checks to ensure there are no conflicts among the field names
validateFieldNames :: Sourceable a => [(Symbol, a)] -> MaybeE [(Symbol, a)]
validateFieldNames = foldr fun (pure [])
                     where fun (s, a) m = m >>= (addIfDone (s, a))

addIfDone :: Sourceable a => (Symbol, a) -> [(Symbol, a)] -> MaybeE [(Symbol, a)]
addIfDone (s, a) lst = if (s `elem`) $ map fst lst
                       then err (src a) ["Duplicate record field names."]
                       else ok $ (s, a):lst
