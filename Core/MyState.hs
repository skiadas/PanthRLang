-- Implementing custom state monad for practice

module MyState where

data MyState s a = MyState {runState :: s -> (a, s) }

instance Functor (MyState s) where
    fmap f m = MyState $ \st -> let (v, st1) = runState m st
                                in (f v, st1)

instance Monad (MyState s) where
    return v = MyState $ \st -> (v, st)
    (>>=) f g = MyState $ \st -> let (v, st1) = runState f st
                                 in runState (g v) st1

getValue :: (a, b) -> a
getValue (a, b) = a

getState :: MyState s s
getState = MyState (\st -> (st, st))

putState :: s -> MyState s ()
putState st = MyState (\_ -> ((), st))

sequenceStates :: [MyState s a] -> MyState s [a]
sequenceStates = foldr fun (return []) where
                        fun f fl = f >>= (\el -> (fmap (el:)) fl)