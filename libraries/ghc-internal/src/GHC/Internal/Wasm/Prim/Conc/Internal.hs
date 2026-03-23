{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Wasm.Prim.Conc.Internal (
  threadDelay
) where

import GHC.Internal.Base
import GHC.Internal.IO

-- See W6 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Wasm.Prim.Imports ()

foreign import javascript safe "new Promise(res => setTimeout(res, $1 / 1000))"
  js_delay :: Int -> IO ()

-- See Note [threadDelay on wasm] for details.
threadDelay :: Int -> IO ()
threadDelay t = evaluate =<< js_delay t
