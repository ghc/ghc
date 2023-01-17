{-# LANGUAGE DataKinds #-}
module T22785 where
import Data.Proxy
import Data.Tuple


p :: Proxy ('MkSolo Int)
p = Proxy :: Proxy Int
