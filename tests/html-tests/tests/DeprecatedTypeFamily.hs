{-# LANGUAGE TypeFamilies #-}
module DeprecatedTypeFamily where

-- | some documentation
data family SomeTypeFamily k :: * -> *
{-# DEPRECATED SomeTypeFamily "SomeTypeFamily" #-}

data family SomeOtherTypeFamily k :: * -> *
{-# DEPRECATED SomeOtherTypeFamily "SomeOtherTypeFamily" #-}
