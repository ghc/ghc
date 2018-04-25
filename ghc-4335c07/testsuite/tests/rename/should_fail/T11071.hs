module T11071 where

import Data.List (lines)
import qualified Data.Map as M ()
import qualified Data.IntMap as M ()
import qualified Data.IntMap as M () -- just to see if this confused the code

import qualified Data.Ord as Ord hiding (Down)

import qualified Data.Map as M' hiding (size, filter)
import qualified Data.Map as M' hiding (size)
import qualified Data.IntMap as M' hiding (size)
import qualified System.IO as M' () -- unrelated

ignore :: a -> IO ()
ignore = const (return ())

main = do
    ignore NoSuchModule.foo  -- no such module
    ignore Data.List.foobar  -- does not exist (one import)
    ignore M.foobar          -- does not exist (two imports)
    ignore M'.foobar         -- does not exist (three imports)
    ignore Data.List.sort    -- needs import
    ignore Data.List.unlines -- needs import, similar to imported
    ignore M.size            -- multiple modules to import from
    ignore M.valid           -- only one module to import from
    ignore Ord.Down          -- explicit hiding
    ignore M'.size           -- hidden and/or missing in import list
