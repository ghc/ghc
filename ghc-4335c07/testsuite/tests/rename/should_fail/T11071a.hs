module T11071 where

import Data.List (lines)
import Data.IntMap ()
import Data.Ord hiding (Down)
import Prelude hiding (True)

ignore :: a -> IO ()
ignore = const (return ())

main = do
    ignore intersperse       -- missing in import list (one import)
    ignore foldl'            -- missing in import list (two imports)
    ignore Down              -- explicitly hidden
    ignore True              -- explicitly hidden from prelude (not really special)
    ignore foobar            -- genuinely out of scope
