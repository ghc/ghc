{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}


module T27994b where
import Data.Proxy

type family G a where G Int = Char
type family F a b = r | r -> a
type instance F a   [c]  = (a, G a)      -- instance 1
type instance F Int Bool = (Int, Bool)   -- instance 2

h :: Proxy a -> F a Bool
h = h
k :: (Int, b) -> b
k = k

test = k (h Proxy) `seq` ()   -- [W] F alpha Bool ~ (Int, gamma)
