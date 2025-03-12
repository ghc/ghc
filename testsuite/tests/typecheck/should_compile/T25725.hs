{-# LANGUAGE TypeFamilies, PolyKinds, UnliftedNewtypes #-}

module T25725 where

import Data.Kind
import GHC.Exts

--This one was OK
data D :: TYPE r -> Type where
  MkD :: p -> D p

-- now this is OK too
data family Dix4 :: Type -> k
data instance Dix4 Int :: TYPE r -> Type where
  DIn4 :: p -> Dix4 Int p

data family Dix5 :: TYPE r -> TYPE r
newtype instance Dix5 f :: TYPE c where
  DIn5 :: g -> (Dix5 g :: TYPE k)

-- data family Dix6 :: TYPE r -> TYPE r
-- newtype instance Dix6 f :: TYPE r where
--   DIn6 :: l -> Dix6 l

-- this one would be rejected
--  in data con sig, we have (Dix6 f) :: Type,
--  but in newtype instance, we have (Dix6 f) :: TYPE r
--  we need coercion between Type and TYPE r
--  which is not possible for newtypes



