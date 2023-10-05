{-# Language RankNTypes #-}
{-# Language TypeApplications #-}
{-# Language DataKinds        #-}
{-# Language PolyKinds        #-}
{-# Language GADTs            #-}
{-# Language TypeFamilies     #-}

module T15787 where

import Data.Kind

class Ríki (ob :: Type) where
 type Hom :: ob -> ob -> Type

type Kl_kind :: forall ob . (ob -> ob) -> ob -> Type
data Kl_kind m k where
  Kl      :: k -> Kl_kind (m :: ob -> ob) k

type family
  UnKl (kl :: Kl_kind m k) = (res :: k) where
  UnKl (Kl a) = a
