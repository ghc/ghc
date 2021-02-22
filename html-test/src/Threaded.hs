{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Ensures haddock built with @-threaded@.
module Threaded where

import Threaded_TH

-- | @$(forkTH)@ fails at compile time if haddock isn't using the
-- threaded RTS.
f = $(forkTH)
