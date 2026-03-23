{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -dno-typeable-binds #-}
  -- See (KKN4) in Note [Overview of known entities]

module Dummy where

import GHC.Internal.Types ()
  -- See Note [Tracking dependencies on primitives] in GHC.Internal.Base
