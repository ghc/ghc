module GHCmain(mainPrimIO) where

import ST
import STBase

mainPrimIO = _ccall_ puts "123\n" >> return ()
