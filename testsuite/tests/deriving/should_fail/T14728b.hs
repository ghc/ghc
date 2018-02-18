{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14728b where

import Data.Functor.Identity
import Data.Kind

class C (a :: Type) where
  type U z :: a

instance C () where
  type U z = '()

deriving instance C (Identity a)
