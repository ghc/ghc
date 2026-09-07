{-# LANGUAGE TemplateHaskell #-}
module Slow where

import Control.Concurrent (threadDelay)
import Language.Haskell.TH

-- A splice that sleeps 60s, so that compiling this module times out.
$(runIO (threadDelay 60000000) >> return [])
