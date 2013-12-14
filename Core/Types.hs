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
          | LetE (Pattern, Expr) Expr   -- Acts as let*
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

data Type = BoolT | IntT | DblT | StrT   -- Base types
          | RecT [(Symbol, Type)]       -- Records/Tuples
          | VectorT Type
          | FunT Type Type
          | VarT Symbol        -- For type variables, types yet to be determined
          | Top
          deriving (Eq, Show)

-- Patterns
data Pattern = VarP Symbol                -- Variable
             | RecP [(Symbol, Pattern)]   -- Record / Tuple
             | WildP                      -- Wildcard
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

makeVector = VectorE
makeIf     = IfE
makeBool   = BoolE
makeInt    = IntE . toInteger
makeDouble = DblE
makeVar    = VarE . toSymbol


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
makeRecord = RecE . validateFieldNames . map (\(s,v) -> (toSymbol s, v)) 

makeTuple :: [Expr] -> Expr
makeTuple lst = RecE $ zip (map show [1..(length lst)]) lst

makeTuplePat :: [Pattern] -> Pattern
makeTuplePat lst = RecP $ zip (map show [1..(length lst)]) lst

makeRecPat :: [(String, Pattern)] -> Pattern
makeRecPat lst = RecP $ zip (map toSymbol ss) pats where (ss, pats) = unzip lst

makeVarPat :: String -> Pattern
makeVarPat = VarP . toSymbol

makeFieldAccess :: Expr -> [Symbol] -> Expr
makeFieldAccess = foldl FieldE

-- "makeCall f argsList"  creates a curried application of f on each arg in order
makeCall :: Expr -> [Expr] -> Expr
makeCall = foldl CallE 

validate :: (a -> Bool) -> String -> a -> a
validate f err a = if (f a) then a else error err

-- Checks to ensure there are no conflicts among the field names
validateFieldNames :: [(Symbol, a)] -> [(Symbol, a)]
validateFieldNames = validate (uniqueSymbols . fst . unzip)
                              "Duplicate record field names."

makeLet :: [(Pattern, Expr)] -> Expr -> Expr
makeLet bs e = foldr LetE e bs

-- Checks if its first argument contains a symbol
contains :: Symbol -> Type -> Bool
contains s (VarT s2) = s2 == s
contains s (RecT lst) = any ((contains s) . snd) lst
contains s (VectorT t) = contains s t
contains s (FunT e1 e2) = (contains s e1) && (contains s e2)
contains s _ = False
