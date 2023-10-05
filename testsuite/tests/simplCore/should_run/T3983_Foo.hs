module T3983_Foo where

import Control.Monad (unless)
import Control.Exception
import T3983_Bar

foo :: Bool -> Bool -> IO ()
foo a b = unless a $ throwX (if b then "" else "")
