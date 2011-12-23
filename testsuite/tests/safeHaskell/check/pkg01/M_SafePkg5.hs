-- safe inference
-- same module as M_SafePkg4 which compiles with -XSafe.
-- Want to make sure compiles fine and is infered safe and
-- also picks up corrected pkg trust requirements.
module M_SafePkg5 where

import qualified M_SafePkg3 as M3
import Data.Word

bigInt :: Int
bigInt = M3.bigInt

type MyWord = Word

