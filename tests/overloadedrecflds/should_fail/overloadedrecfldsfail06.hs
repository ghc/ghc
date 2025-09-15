-- Check that unused imports are reported correctly in the presence of
-- DuplicateRecordFields

{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror -fwarn-unused-imports #-}

import OverloadedRecFldsFail06_A (U(x, y), V(MkV, MkV2, x, y), Unused(unused), u, getY)
import qualified OverloadedRecFldsFail06_A as M (U(x))
import qualified OverloadedRecFldsFail06_A as N (V(x, y))
import qualified OverloadedRecFldsFail06_A as P (U(x), V(x))

v = MkV2 True

-- Check that this counts a use of U(x) and V(y) but not U(y) or V(x)...
main = do print (u { x = True } :: U)
          print ((\ MkV2{y=y} -> y) v)
          print (N.x v)
          print (getY (v { P.x = 3 }))
