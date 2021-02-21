{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module T13163
  ( Record(..)
  , type (?)(..)
  , f, type (+), pattern Single
  ) where

import Data.Promotion.Prelude (type (:+$), type (:*$), type (:^$), type (:-$))
import Options.Generic (Generic, ParseRecord, type (<?>)(..))
import GHC.TypeLits

pattern Single x = [x]

f = undefined
