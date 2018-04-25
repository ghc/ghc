module Test where

import Distribution.TestSuite
import Child

tests :: IO [Test]
tests = return $ [Test $ TestInstance
          { run = return (Finished Pass)
          , name = "test"
          , tags = []
          , options = []
          , setOption = \_ _-> Left "No Options"
          }]
