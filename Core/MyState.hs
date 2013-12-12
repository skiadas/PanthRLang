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



sequenceStates :: [MyState s a] -> MyState s [a]
sequenceStates = foldr fun (return []) where
                        fun f fl = f >>= (\el -> (fmap (el:)) fl)