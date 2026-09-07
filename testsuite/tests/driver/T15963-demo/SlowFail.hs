{-# LANGUAGE TemplateHaskell #-}
module SlowFail where

import Control.Concurrent (threadDelay)
import Language.Haskell.TH

-- Sleeps 60s and then fails, so the compile_fail test times out first.
$(runIO (threadDelay 60000000) >> fail "never reached")
