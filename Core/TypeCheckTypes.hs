module TypeCheckTypes where

import Utils
import Types
import Env
import MyState

 -- Used when the symbol represents program variables
data Context = Cntx {ctx :: Env Type }

instance Show Context where
    show (Cntx env) = show env

extendCtx :: Symbol -> Type -> Context -> Context
extendCtx s ty (Cntx env) = Cntx $ extend env s ty

emptyContext = Cntx emptyEnv

 -- Used when the symbol represents an abstract variable type
data TypeSubst = TSubst { getSubst :: Env Type } deriving (Show)

type Constraints = [(Type, Type)]

emptyConstraints = [] :: Constraints

data TypingState = TypingSt {
        getContext :: Context,
        getConstraints :: Constraints,
        getGen :: SymbolGen }

instance Show TypingState where
    show (TypingSt ctx cons _) = (show ctx) ++ ('\n' : show cons)

type TypingStMonad = MyState TypingState

emptyState :: TypingState
emptyState = TypingSt emptyContext emptyConstraints seedGen

liftCtx :: (Context -> Context) -> TypingState -> TypingState
liftCtx f (TypingSt a b c) = TypingSt (f a) b c

liftCons :: (Constraints -> Constraints) -> TypingState -> TypingState
liftCons f (TypingSt a b c) = TypingSt a (f b) c

getFreeSymbol :: TypingState -> (Symbol, TypingState)
getFreeSymbol (TypingSt ctx cons gen) = (sym gen, TypingSt ctx cons (next gen))

freeType :: TypingStMonad Type
freeType = MyState ((joinF VarT id) . getFreeSymbol)


-- Monad actions pertaining to this specific monad
addConstraintList :: Constraints -> TypingStMonad ()
addConstraintList lst =  updateState $ liftCons ((++) lst)
