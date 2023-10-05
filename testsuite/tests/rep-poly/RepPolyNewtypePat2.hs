{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedNewtypes #-}

module RepPolyNewtypePat2 where

import Data.Kind
import GHC.Exts

type X :: TYPE rep -> TYPE rep
newtype X a = MkX a

bar :: forall (a :: Type). X a -> a
bar (MkX bndr_a) = bndr_a
