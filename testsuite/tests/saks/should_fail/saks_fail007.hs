{-# LANGUAGE StandaloneKindSignatures #-}

module SAKS_Fail007 where

import Data.Kind (Type)

type May a :: Type
data May a = Nay | Yay a
