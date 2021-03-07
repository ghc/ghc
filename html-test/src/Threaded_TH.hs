{-# LANGUAGE Haskell2010 #-}
-- | Imported by 'Threaded', since a TH splice can't be used in the
-- module where it is defined.
module Threaded_TH where

import Control.Concurrent (forkOS)
import Language.Haskell.TH.Syntax (Exp (LitE), Lit (IntegerL), Q, runIO)

-- | forkOS requires the threaded RTS, so this TH fails if haddock was
-- built without @-threaded@.
forkTH :: Q Exp
forkTH = do
  _ <- runIO (forkOS (return ()))
  return (LitE (IntegerL 0))
