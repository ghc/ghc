{-# LANGUAGE EmptyDataDecls, TypeFamilies, TypeOperators, ScopedTypeVariables #-}
module Overflow where
import Control.Concurrent

data (:*:) a b
data (:+:) a b

data family PChan a
data    instance PChan (a :+: b) = E (IO (PChan a)) (IO (PChan b))
newtype instance PChan (a :*: b) = O (IO (Either (PChan a) (PChan b)))

type family Dual a
type instance Dual (a :+: b) = Dual a :*: Dual b
type instance Dual (a :*: b) = Dual a :+: Dual b

class Connect s where
    newPChan :: (s ~ Dual c, c ~ Dual s) => IO (PChan s, PChan c)

pchoose :: (t -> a) -> MVar a -> IO (t,b) -> IO b
pchoose = undefined

instance (Connect a, Connect b) => Connect (a :*: b) where
    newPChan = do
        v <- newEmptyMVar
        -- correct implementation:
        return (O $ takeMVar v, E (pchoose Left v newPChan) (pchoose Right v newPChan))
       
        -- This version is in T2664
        -- type error leads to stack overflow (even without UndecidableInstances!)
        --return (O $ takeMVar v, E (pchoose Right v newPChan) (pchoose Left v newPChan))
