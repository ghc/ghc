{-# LANGUAGE DataKinds, UndecidableInstances #-}
module T1 where
import GHC.TypeLits


data MyType = MyType

instance
  TypeError (Text "Values of type 'MyType' cannot be compared for equality.")
    => Eq MyType where (==) = undefined

err x = x == MyType


