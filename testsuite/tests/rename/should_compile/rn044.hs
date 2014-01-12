-- This one should succeed; x is unambiguous, even
-- though M.x is not

module ShouldCompile (module M) where

import qualified Rn044_A as M  -- M.x
import Rn044_B as M            -- x, M.x

