{-# LANGUAGE KindSignatures #-}
module T808 where

import Data.Kind (Type)

foo :: (Int, Int :: Type)
foo = undefined
