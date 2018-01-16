module Lib where

import Distribution.TestSuite

nullt x = Test $ TestInstance
          { run = return $ Finished (Fail "no reason")
          , name = "test " ++ show x
          , tags = []
          , options = []
          , setOption = \_ _-> Left "No Options"
          }
