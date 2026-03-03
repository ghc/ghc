{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module T26941_aux where

import Data.Kind
import GHC.TypeLits

shxHead :: ListH (n : sh) i -> SMayNat i n
shxHead list = {-# SCC "bad_scc" #-}
  ( case list of (i `ConsKnown` _) -> SKnown i )

type ListH :: [Maybe Nat] -> Type -> Type
data ListH sh i where
  ConsKnown :: SNat n -> ListH sh i -> ListH (Just n : sh) i

data SMayNat i n where
  SKnown :: SNat n -> SMayNat i (Just n)
