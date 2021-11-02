{-# LANGUAGE NoImplicitPrelude #-}

module GHC.ExecutionStack
  ( Location,
    getStackTrace,
  )
where

import GHC.Base
import {-# SOURCE #-} GHC.ExecutionStack.Internal (Location)

getStackTrace :: IO (Maybe [Location])
