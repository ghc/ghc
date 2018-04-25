module UnitTests.Distribution.Simple.Program.Internal
    ( tests
    ) where

import Distribution.Simple.Program.Internal ( stripExtractVersion )

import Test.Tasty
import Test.Tasty.HUnit

v :: String
v = "GNU strip (GNU Binutils; openSUSE 13.2) 2.24.0.20140403-6.1\nCopyright 2013\
  \ Free Software Foundation, Inc.\nThis program is free software; you may\
  \ redistribute it under the terms of\nthe GNU General Public License version 3\
  \ or (at your option) any later version.\nThis program has absolutely no\
  \ warranty.\n"

v' :: String
v' = "GNU strip 2.17.50.0.6-26.el5 20061020"

v'' :: String
v'' = "GNU strip (openSUSE-13.2) 2.23.50.0.6-26.el5 20061020"

v''' :: String
v''' = "GNU strip (GNU (Binutils for) Ubuntu 12.04 ) 2.22"

tests :: [TestTree]
tests =
    [ testCase "Handles parentheses" $
      (stripExtractVersion v)    @=? "2.24"
    , testCase "Handles dashes and alphabetic characters" $
      (stripExtractVersion v')   @=? "2.17"
    , testCase "Handles single-word parenthetical expressions" $
      (stripExtractVersion v'')  @=? "2.23"
    , testCase "Handles nested parentheses" $
      (stripExtractVersion v''') @=? "2.22"
    ]
