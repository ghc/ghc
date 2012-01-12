{-# LANGUAGE TypeFamilies #-}
module DeprecatedData where

-- | type Foo
data Foo = Foo -- ^ constructor Foo
         | Bar -- ^ constructor Bar

{-# DEPRECATED Foo "Foo" #-}
{-# DEPRECATED Bar "Bar" #-}

data One = One
         | Two

{-# DEPRECATED One "One" #-}
{-# DEPRECATED Two "Two" #-}
