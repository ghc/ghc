{-# LANGUAGE OverloadedRecordFields #-}

import OverloadedRecFldsRun12_B (F(MkFInt, MkFBool, foo))

main = do print (foo (MkFInt 42))
          print (foo (MkFBool True))
