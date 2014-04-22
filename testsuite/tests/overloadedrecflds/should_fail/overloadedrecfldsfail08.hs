{-# LANGUAGE OverloadedRecordFields #-}

import OverloadedRecFldsFail08_A

-- Testing bogus instances (for universally or existentially
-- quantified field types) imported from another module
a = x (MkT True) :: Bool
b = y (MkU id)
c = z (MkU2 (\ _ -> ()))
d = foo (MkFInt 42)
e = foo (MkFBool id)

main = return ()
