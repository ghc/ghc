module Main where

import GHC.Driver.Session

import Control.Monad
import Data.List (isPrefixOf)

-- Verify bogus flags aren't printed on flagsForCompletion and
-- allNonDeprecatedFlags:
--  * -fwarn-
--  * -fno-warn-
--
-- Should print nothing
main :: IO ()
main = mapM_ print $ fwarnFlags (flagsForCompletion True) ++ nonDepFwarnFlags

-- Get flags beginning with -fwarn- and -fno-warn-
fwarnFlags :: [String] -> [String]
fwarnFlags = filter isFwarn
  where isFwarn flag = any (flip isPrefixOf $ flag) ["-fwarn-", "-fno-warn"]

-- Get suggested flags for -fwarn-, -fno-warn-
nonDepFwarnFlags :: [String]
nonDepFwarnFlags = filter isFwarn allNonDeprecatedFlags
  where isFwarn "-fwarn-"    = True
        isFwarn "-fno-warn-" = True
        isFwarn _            = False
