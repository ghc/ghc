{-# language TypeInType, EmptyCase #-}
module T14086 where
import Data.Kind

f :: Type -> Int
f x = case x of
