module Foo where

import Distribution.TestSuite

tests :: IO [Test]
tests = return [Test $ TestInstance
          { run = return (Finished (Fail "A"))
          , name = "test A"
          , tags = []
          , options = []
          , setOption = \_ _-> Left "No Options"
          }]
