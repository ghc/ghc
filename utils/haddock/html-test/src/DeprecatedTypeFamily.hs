{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
module DeprecatedTypeFamily where

import Data.Kind (Type)

-- | some documentation
data family SomeTypeFamily k :: Type -> Type
{-# DEPRECATED SomeTypeFamily "SomeTypeFamily" #-}

data family SomeOtherTypeFamily k :: Type -> Type
{-# DEPRECATED SomeOtherTypeFamily "SomeOtherTypeFamily" #-}
