module DeprecatedNewtype where

-- | some documentation
newtype SomeNewType = SomeNewTypeConst String {- ^ constructor docu -}
{-# DEPRECATED SomeNewType "SomeNewType" #-}
{-# DEPRECATED SomeNewTypeConst "SomeNewTypeConst" #-}

newtype SomeOtherNewType = SomeOtherNewTypeConst String
{-# DEPRECATED SomeOtherNewType "SomeOtherNewType" #-}
{-# DEPRECATED SomeOtherNewTypeConst "SomeOtherNewTypeConst" #-}
