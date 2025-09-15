{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module T7269 where

class C (a :: k)

instance C Int

newtype MyInt = MyInt Int deriving C

newtype YourInt = YourInt Int
deriving instance C YourInt
