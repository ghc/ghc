-- |
-- What is tested here:
--
-- * Deprecation messages are shown for re-exported items.
--
module DeprecatedReExport (
-- * Re-exported from an other module
  foo
-- * Re-exported from an other package
-- | Not yet working, see <http://trac.haskell.org/haddock/ticket/223>
-- , isEmptyChan
,
) where

import DeprecatedFunction
import Control.Concurrent.Chan
