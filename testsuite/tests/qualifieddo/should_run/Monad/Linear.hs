{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
module Monad.Linear where

import Prelude(Int, (+))

data T where T :: Int -> T
data TM a = TM a

class Monad m where
  return :: a #-> m a
  (>>=) :: m a #-> (a #-> m b) #-> m b

(>>) :: Monad m => m () #-> m b #-> m b
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

increaseT :: T #-> TM T
increaseT (T i) = return (T (i+1))

extractT :: T #-> TM (T, Unrestricted Int)
extractT (T i) = return (T i, Unrestricted i)

deleteT :: T #-> TM ()
deleteT (T _) = return ()
