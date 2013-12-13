-- Core types of PanthR language

module Types where
    
import Env

data Info = None -- Temporary placeholder. This type meant to contain parsing information

data SourceExpr = SExpr Info Expr

data Expr = IntE Integer | DblE Double | BoolE Bool | StrE String
          | VectorE [Expr]
          | ArithmE ArithmOp Expr Expr
          | LogicalE LogicOp Expr Expr
          | CompareE CompareOp Expr Expr -- Binary Ops
          | NegateE Expr | NotE Expr -- Unary Ops
          | IfE Expr Expr Expr   -- if-then-else
          | FunE Function
          | VarE Symbol
          | CallE Expr Expr
          | RecE [(Symbol, Expr)] -- Records. Order does not matter. Doubling as tuples
          | FieldE Expr Symbol    -- Record field access.
          deriving (Show)

-- Binary operators
data ArithmOp = OpPlus | OpMinus | OpMult | OpDivide deriving (Eq, Show)
data LogicOp = OpAnd | OpOr deriving (Eq, Show)
data CompareOp = OpGreater | OpLess | OpGeq | OpLeq | OpEq | OpNeq deriving (Eq, Show)

data Function = LambdaE Symbol Expr | BuiltInE (Value -> Value)

instance Show Function where
    show _ = "function"
-- Data type for possible values
data Value = IntV Integer | DblV Double | BoolV Bool | StrV String
           | VectorV [Value]
           | ClosV (Env Value) Function
           | RecV [(Symbol, Value)]
           deriving (Show)

arithmOps = [(OpPlus, "+", (+)),
             (OpMinus, "-", (-)),
             (OpMult, "*", (*)),
             (OpDivide, "/", (/))]

intFunFromArithmOp :: ArithmOp -> Integer -> Integer -> Integer
intFunFromArithmOp OpPlus = (+)
intFunFromArithmOp OpMinus = (-)
intFunFromArithmOp OpMult = (*)

dblFunFromArithmOp :: ArithmOp -> Double -> Double -> Double
dblFunFromArithmOp OpPlus = (+)
dblFunFromArithmOp OpMinus = (-)
dblFunFromArithmOp OpMult = (*)
dblFunFromArithmOp OpDivide = (/)

toVar :: String -> Expr
toVar = VarE . toSymbol

toDouble :: Double -> Expr
toDouble = DblE

toInt :: Integral a => a -> Expr
toInt = IntE . toInteger

toBool :: Bool -> Expr
toBool = BoolE

toString :: String -> Expr
toString = StrE

ensureBool :: Value -> Value
ensureBool b@(BoolV _) = b
ensureBool e = error ("Expected a boolean, got: " ++ show e)

valToBool :: Value -> Bool
valToBool (BoolV b) = b
valToBool _ = error "Trying to convert non-boolean to boolean"

-- applyOp :: (Num a) => DuOp -> (a -> a)
applyOp OpGreater = (>)
applyOp OpLess    = (<)
applyOp OpGeq     = (>=)
applyOp OpLeq     = (<=)
applyOp OpEq      = (==)
applyOp OpNeq     = (/=)

makeLambda :: [String] -> Expr -> Expr
makeLambda [] body = body
makeLambda (s:rest) body = FunE $ LambdaE (toSymbol s) (makeLambda rest body)

makeField :: Expr -> String -> Expr
makeField obj s = FieldE obj (toSymbol s)

makeRecord :: [(String, Expr)] -> Expr
makeRecord = RecE . map (\(s,v) -> (toSymbol s, v)) 

makeTuple :: [Expr] -> Expr
makeTuple lst = RecE $ zip (map show [1..(length lst)]) lst
