module BuiltIns where
    
import Syntax
import Env(Symbol, emptyEnv, extendList, makeSymbol)

closeBuiltIn g = ClosV emptyEnv $ BuiltInE g

liftDblDbl :: (Double -> Double) -> Value
liftDblDbl f = closeBuiltIn g where
                    g (DblV d) = (DblV $ f d)
                    g (IntV d) = g (DblV $ fromIntegral d)
                    g _ = error "Expected double in built-in"

liftIntIntInt :: (Integer -> Integer -> Integer) -> Value
liftIntIntInt f = closeBuiltIn g where
                    g (IntV d) = closeBuiltIn (h d)
                    g _ = error "Expected integer in built-in"
                    h d (IntV d2) = IntV $ f d d2
                    h _ _ = error "Expected integer in built-in"

liftDblInt :: (Double -> Integer) -> Value
liftDblInt f = closeBuiltIn g where
                    g (DblV d) = (IntV $ f d)
                    g (IntV d) = g (DblV $ fromIntegral d)
                    g _ = error "Expected double in built-in"

liftIntDbl :: (Integer -> Double) -> Value
liftIntDbl f = closeBuiltIn g where
                    g (IntV d) = (DblV $ f $ fromIntegral d)
                    g _ = error "Expected integer in built-in"


liftDblDblDbl :: (Double -> Double -> Double) -> Value
liftDblDblDbl f = closeBuiltIn g where
                    g (DblV d) = closeBuiltIn (h d)
                    g (IntV d) = g (DblV $ fromIntegral d)
                    g _ = error "Expected double in built-in"
                    h d (DblV d2) = DblV $ f d d2
                    h d (IntV d2) = h d (DblV $ fromIntegral d2)
                    h _ _ = error "Expected double in built-in"

liftEnv :: (a -> Value) -> (String, a) -> (Symbol, Value)
liftEnv g (s, f) = (makeSymbol s, g f)


builtIns      =  map (liftEnv id) [("pi", DblV pi), ("e", DblV (exp 1))]
              ++ map (liftEnv liftIntIntInt)
                    [("div", div), ("mod", mod), ("gcd", gcd), ("lcm", lcm)]
              ++ map (liftEnv liftDblInt)
                    [("ceiling", ceiling), ("floor", floor), ("round", round)]
              ++ map (liftEnv liftDblDblDbl)
                    [("pow", (**)), ("logBase", logBase)]
              ++ map (liftEnv liftDblDbl)
                    [ ("exp", exp), ("sqrt", sqrt), ("log", log)
                    , ("sin", sin), ("tan", tan), ("cos", cos)
                    , ("asin", asin), ("atan", atan), ("acos", acos)
                    , ("sinh", sinh), ("tanh", tanh), ("cosh", cosh)
                    , ("asinh", asinh), ("atanh", atanh), ("acosh", acosh)]

builtInEnv = extendList emptyEnv builtIns