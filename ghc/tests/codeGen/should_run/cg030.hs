module GHCmain(mainIO) where

import ST
import STBase

mainIO = _ccall_ puts "123\n" >> return ()
