-- Modules A and B both declare F(foo)
-- Module C declares F($sel:foo:MkFChar) but exports A.F(foo) as well
-- Thus we can't export F(..) even with DuplicateRecordFields enabled

{-# LANGUAGE DuplicateRecordFields #-}
module Main (main, F(..)) where

import OverloadedRecFldsFail10_B
import OverloadedRecFldsFail10_C

main = return ()
