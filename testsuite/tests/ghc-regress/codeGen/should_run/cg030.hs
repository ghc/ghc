module PrelMain(mainIO) where

import GHC.ST
import Data.PackedString

mainIO = _ccall_ puts (packString "123\n") >> return ()
