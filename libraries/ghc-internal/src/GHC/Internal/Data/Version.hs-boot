{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Data.Version
  ( Version
  , makeVersion
  ) where

import GHC.Types


-- The generated module GHC.Internal.Unicode.Version depends on
-- GHC.Internal.Base.build via built-in list syntax, and its only
-- import is this hs-boot file, so until someone fixes the generator,
-- this import has to stay here to ensure sound build ordering.  See also
-- W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base.
import GHC.Internal.Base ()

data Version

makeVersion :: [Int] -> Version
