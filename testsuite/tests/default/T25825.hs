{-# LANGUAGE TypeFamilyDependencies #-}

module T25825 where

import Data.Coerce ( coerce )

type family F a = r | r -> a
type instance F Int = Bool

foo :: F b -> ()
foo _ = ()



bar = foo ( coerce False )
  -- [W] F beta ~R Bool
  --
  -- Check that we default beta := Int. This requires:
  --
  --  - promoting the representational equality to a nominal one
  --    during defaulting, and then,
  --  - using injectivity of F to conclude beta := Int.

