-- Module A exports $fld:MkFInt:foo
-- Module B exports $fld:MkFBool:foo
-- Module C exports $fld:MkFChar:foo and re-exports $fld:MkFInt:foo
-- Thus we can't export F(..) without -XDuplicateRecordFields

module Main (main, F(..)) where

import OverloadedRecFldsFail10_B
import OverloadedRecFldsFail10_C

main = return ()
