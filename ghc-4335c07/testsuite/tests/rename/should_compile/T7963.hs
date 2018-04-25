module T7963 where

import Prelude hiding (unlines)
import T7963a (unlines)

foo :: IO ()
foo = writeFile "/tmp/foo" (unlines ["hello", "world"])
