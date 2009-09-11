module CrossPackageDocs (map, Monad(..), runInteractiveProcess, MVar(..), newEmptyMVar  {- $ Bugs -}) where

import System.Process
import GHC.MVar

-- $ Bugs:
--
--   * [] a
--
--   * No instances list
--
--   * No docs on function arguments
