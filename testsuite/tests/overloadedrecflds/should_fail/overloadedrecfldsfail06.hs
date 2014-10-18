{-# LANGUAGE OverloadedRecordFields #-}
{-# OPTIONS_GHC -Werror -fwarn-unused-imports #-}

import OverloadedRecFldsFail06_A (U(x, y), V(MkV, MkV2, x, y), Unused(unused), u, getX, getY)

foo r = getY r

-- Check that this counts a use of U(x) and V(y) but not U(y) or V(x)
main = do print (getX u)
          print (y (MkV2 True))
