{-# LANGUAGE OverloadedRecordFields #-}

import OverloadedRecFldsRun07_B

main = do print (foo (MkFBool True))
          print (foo (MkFInt 3))
          print (bar (MkFChar 'a'))
