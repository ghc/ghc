{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
module Monad.Linear where

import Prelude(Int, (+))

data T where T :: Int -> T
data TM a = TM a

class Monad m where
  return :: a %1 -> m a
  (>>=) :: m a %1 -> (a %1 -> m b) %1 -> m b

(>>) :: Monad m => m () %1 -> m b %1 -> m b
m1 >> m2 = m1 >>= \() -> m2

instance Monad TM where
  return = TM
  TM a >>= f = f a

data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

runTM :: TM (Unrestricted a) -> a
runTM (TM (Unrestricted a)) = a

newT :: TM T
newT = return (T 0)

increaseT :: T %1 -> TM T
increaseT (T i) = return (T (i+1))

extractT :: T %1 -> TM (T, Unrestricted Int)
extractT (T i) = return (T i, Unrestricted i)

deleteT :: T %1 -> TM ()
deleteT (T _) = return ()
