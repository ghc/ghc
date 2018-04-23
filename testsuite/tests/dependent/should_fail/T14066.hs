{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures #-}

module T14066 where

import Data.Kind ( Type )
import Data.Type.Equality
import Data.Proxy
import GHC.Exts

data SameKind :: k -> k -> Type

f (x :: Proxy a) = let g :: forall k (b :: k). SameKind a b
                       g = undefined
                   in ()
