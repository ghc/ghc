{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies #-}

import GHC.Records (HasField(..))

data T = MkT { foo :: Int, bar :: Int }

-- This is far too polymorphic
instance HasField "woo" a Bool where
  getField = const True

-- This conflicts with the built-in instance
instance HasField "foo" T Int where
  getField = foo

-- So does this
instance HasField "bar" T Bool where
  getField = const True

-- This doesn't conflict because there is no "baz" field in T
instance HasField "baz" T Bool where
  getField = const True

-- Bool has no fields, so this is okay
instance HasField a Bool Bool where
  getField = id


data family V a b c d
data instance V x Int y [z] = MkVInt { baz :: (x, y, z, Bool) }

-- Data families cannot have HasField instances, because they may get
-- fields defined later on
instance HasField "baz" (V a b c d) Bool where
  getField = const True

-- Function types can have HasField instances, in case it's useful
instance HasField "woo" (a -> b) Bool where
  getField = const True
