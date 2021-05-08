-- | The occurrence of 'notVisible' should not be reported as ambiguous
-- as it is not visible from the @hs-boot@ file.
module T19532 (hello, visible) where

import {-# SOURCE #-} T19352a
import T19352b

hello :: String
hello = notVisible

