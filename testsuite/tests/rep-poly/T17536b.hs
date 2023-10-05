

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T17536b where

import Data.Kind
import GHC.Exts

type P :: forall (r :: RuntimeRep). TYPE r -> Type
data P a where
  L :: P Int
  U :: P Int#

g :: forall r (a :: TYPE r). P a -> a -> a
g L = \ _ -> 0
g U = \ _ -> 3#
