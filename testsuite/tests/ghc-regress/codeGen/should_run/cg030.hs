module PrelMain(mainIO) where

import ST
import CString

mainIO = _ccall_ puts (packString "123\n") >> return ()
