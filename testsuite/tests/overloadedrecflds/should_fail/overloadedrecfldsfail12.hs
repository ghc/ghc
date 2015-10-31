{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror #-}

import OverloadedRecFldsFail12_A

data S = MkS { foo :: Bool }

-- Use of foo and bar should give deprecation warnings
f :: T -> T
f e = e { foo = 3, bar = 3 }

s :: T -> Int
s = foo

main = return ()
