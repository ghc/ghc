--
-- Must have rules off to avoid them making it look lazy when it isn't really
--

import Control.Monad
import System.Exit
import Test.HUnit

import qualified Data.ByteString.Lazy as L

------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests :: Test
tests = TestList [append_tests]

append_tests :: Test
append_tests = TestLabel "append" $ TestList [
    TestLabel "lazy tail" $ TestCase $
        L.head (L.pack [38] `L.append` error "Tail should be lazy") @=? 38
   ]

