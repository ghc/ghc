{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module T16762b where

import Data.Kind

type T :: forall k. Type
data T
