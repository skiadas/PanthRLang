-- Core types of PanthR language

module Types where
    
import Env

data Info = None -- Temporary placeholder. This type meant to contain parsing information

data SourceExpr = SExpr Info Expr

data TExpr = Typed { typ :: Type, expr :: Expr } deriving (Show)

data Expr = IntE Integer | DblE Double | BoolE Bool | StrE String
          | VectorE [TExpr]
          | ArithmE ArithmOp TExpr TExpr
          | LogicalE LogicOp TExpr TExpr
          | CompareE CompareOp TExpr TExpr -- Binary Ops
          | NegateE TExpr | NotE TExpr -- Unary Ops
          | IfE TExpr TExpr TExpr   -- if-then-else
          | FunE Function
          | VarE Symbol
          | CallE TExpr TExpr
          | RecE [(Symbol, TExpr)] -- Records. Order does not matter. Doubling as tuples
          | FieldE TExpr Symbol    -- Record field access.
          | LetE (Pattern, TExpr) TExpr   -- Acts as let*
          deriving (Show)

-- Binary operators
data ArithmOp = OpPlus | OpMinus | OpMult | OpDivide deriving (Eq, Show)
data LogicOp = OpAnd | OpOr deriving (Eq, Show)
data CompareOp = OpGreater | OpLess | OpGeq | OpLeq | OpEq | OpNeq deriving (Eq, Show)

data Function = LambdaE Symbol TExpr | BuiltInE (Value -> Value)

instance Show Function where
    show _ = "function"

-- Data type for possible values
data Value = IntV Integer | DblV Double | BoolV Bool | StrV String
           | VectorV [Value]
           | ClosV (Env Value) Function
           | RecV [(Symbol, Value)]
           deriving (Show)

data Type = BoolT | IntT | DblT | StrT   -- Base types
          | NumT                         -- Type to encompass all numerical operations
          | RecT [(Symbol, Type)]        -- Records/Tuples
          | VectorT Type
          | FunT Type Type
          | VarT Symbol        -- For type variables, types yet to be determined
          | Top
          | UndefT             -- Undefined type. Non-primitives start that way
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


makeTExpr            = Typed
makeUnTExpr          = Typed UndefT
makeVector           = makeUnTExpr . VectorE
makeIf         a b c = makeUnTExpr $ IfE a b c
makeBool             = (makeTExpr BoolT) . BoolE
makeInt              = (makeTExpr  IntT) . IntE . toInteger
makeDouble           = (makeTExpr  DblT) . DblE
makeString           = (makeTExpr StrT) . StrE
makeVar              = makeUnTExpr . VarE . makeSymbol
makeCompareOp op b c = makeUnTExpr (CompareE op b c)
makeArithmOp  op b c = makeUnTExpr (ArithmE  op b c)
makeLogicalOp op b c = makeUnTExpr (LogicalE op b c)
makeNot              = makeUnTExpr . NotE
makeNegate           = makeUnTExpr . NegateE

makeLambda :: [String] -> TExpr -> TExpr
makeLambda [] body = body
makeLambda [s] body = makeUnTExpr . FunE $ LambdaE (makeSymbol s) body
makeLambda (s:rest) body = makeLambda [s] (makeLambda rest body)

makeField :: TExpr -> String -> TExpr
makeField obj s = makeUnTExpr $ FieldE obj (makeSymbol s)

makeRecord :: [(String, TExpr)] -> TExpr
makeRecord = makeUnTExpr . RecE . validateFieldNames . map (\(s,v) -> (makeSymbol s, v)) 

makeTuple :: [TExpr] -> TExpr
makeTuple lst = makeUnTExpr . RecE $ zip (map (makeSymbol . show) [1..(length lst)]) lst

makeTuplePat :: [Pattern] -> Pattern
makeTuplePat lst = RecP $ zip (map (makeSymbol . show) [1..(length lst)]) lst

makeRecPat :: [(String, Pattern)] -> Pattern
makeRecPat lst = RecP $ zip (map makeSymbol ss) pats where (ss, pats) = unzip lst

makeVarPat :: String -> Pattern
makeVarPat = VarP . makeSymbol

makeFieldAccess :: TExpr -> [String] -> TExpr
makeFieldAccess = foldl (\e s -> Typed UndefT $ FieldE e (makeSymbol s))

-- "makeCall f argsList"  creates a curried application of f on each arg in order
makeCall :: TExpr -> [TExpr] -> TExpr
makeCall = foldl (\f arg -> (Typed UndefT $ CallE f arg))

makeLet :: [(Pattern, TExpr)] -> TExpr -> TExpr
makeLet bs e = foldr (\b e -> Typed UndefT $ LetE b e) e bs




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

validate :: (a -> Bool) -> String -> a -> a
validate f err a = if (f a) then a else error err

-- Checks to ensure there are no conflicts among the field names
validateFieldNames :: [(Symbol, a)] -> [(Symbol, a)]
validateFieldNames = validate (uniqueSymbols . fst . unzip)
                              "Duplicate record field names."

-- Checks if its first argument contains a symbol
contains :: Symbol -> Type -> Bool
contains s (VarT s2) = s2 == s
contains s (RecT lst) = any ((contains s) . snd) lst
contains s (VectorT t) = contains s t
contains s (FunT e1 e2) = (contains s e1) && (contains s e2)
contains s _ = False
