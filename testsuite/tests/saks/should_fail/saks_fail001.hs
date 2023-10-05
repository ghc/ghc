{-# LANGUAGE Haskell2010 #-}
-- No -XStandaloneKindSignatures!

module SAKS_Fail001 where

import Data.Kind (Type)

type T :: Type
data T
