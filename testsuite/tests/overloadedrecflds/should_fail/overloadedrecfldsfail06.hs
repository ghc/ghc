-- Check that unused imports are reported correctly in the presence of
-- AllowDuplicateRecordFields

{-# LANGUAGE AllowDuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror -fwarn-unused-imports #-}

import OverloadedRecFldsFail06_A (U(MkU, x, y), V(MkV, MkV2, x, y), Unused(unused), u, getX, getY)

v = MkV2 True

-- Check that this counts a use of U(x) and V(y) but not U(y) or V(x)
main = do print (getX u)
          print (getY v)
          print ((\ MkU{x=x} -> x) u)
          print ((\ MkV2{y=y} -> y) v)
