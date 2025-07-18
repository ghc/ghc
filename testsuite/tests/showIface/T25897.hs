-- | Re-exports should display `b` first and `a` second, as per the explicit
-- exports of that module.
module T25897
  ( module T25897c
  , module T25897b
  ) where

import T25897b
import T25897c
