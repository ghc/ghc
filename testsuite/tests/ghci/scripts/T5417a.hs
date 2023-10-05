{-# LANGUAGE TypeFamilies #-}
module T5417a where

  import Data.Kind (Type)

  class C1 a where
    data F a :: Type
