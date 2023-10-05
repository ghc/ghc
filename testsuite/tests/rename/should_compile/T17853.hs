{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Werror=unused-imports #-}
module T17853 where

-- All the imports of T17853A are necessary, so they should not be reported as
-- redundant.  DisambiguateRecordFields has special logic for looking up field
-- labels in record field construction because the module qualifier is optional.
-- Previously this incorrectly reported imports as redundant if they were used
-- only for fields that were in scope under a different prefix (see #17853).
import qualified T17853A
import qualified T17853A as X (X(..))
import qualified T17853A as Y (Y(..))

main :: IO ()
main = do
    print T17853A.X { X.name = "hello" }
    print T17853A.Y { Y.age = 3 }
