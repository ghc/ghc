{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T25725 where

import Data.Kind
import GHC.Exts

--This one was OK
data D :: TYPE r -> Type where
  MkD :: p -> D p

-- But this was rejected
data family Dix4 :: Type -> k
data instance Dix4 Int :: TYPE r -> Type where
  DIn4 :: p -> Dix4 Int p


