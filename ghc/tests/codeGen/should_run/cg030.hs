module PrelMain(mainIO) where

import ST

mainIO = _ccall_ puts "123\n" >> return ()
