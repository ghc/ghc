{-# LANGUAGE TemplateHaskell #-}
module T24886 where

import GHC.Exts.Heap
import Control.Monad.IO.Class

-- this is a TH splice importing from ghc-heap, testing that the JS linker
-- can find the ghc-heap package during TH evaluation (see #24886)
do
  let !_b = asBox "foo"
  liftIO $ putStrLn "ok"
  return []
