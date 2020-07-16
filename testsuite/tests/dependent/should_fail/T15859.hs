{-# Language PolyKinds          #-}
{-# Language TypeApplications   #-}
{-# Language LiberalTypeSynonyms #-}

module T15859 where

import Data.Kind

a = (undefined :: forall k -> k -> Type) @Int

