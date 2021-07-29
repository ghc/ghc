{-# LANGUAGE DataKinds #-}

module T20181 where

import GHC.TypeLits( TypeError, ErrorMessage(..) )

-- This should be fine
type Foo = TypeError (Text "foo")
