import IO

import System (getArgs)
import Char   (toUpper)
import Directory (removeFile, doesFileExist)

main   =  getArgs                           >>=        \ [f1,f2] ->
          openFile f1 ReadMode              >>=        \ h1      ->
          doesFileExist f2                  >>=        \ f       ->
          if f then removeFile f2 else return () >>
          openFile f2 WriteMode             >>=        \ h2      ->
          copyFile h1 h2                    >>
          hClose h1                         >>
          hClose h2

copyFile h1 h2 =
          hIsEOF h1                         >>=        \ eof ->
          if eof then
            return ()
          else
            hGetChar h1                     >>=        \ c       ->
            hPutChar h2 (toUpper c)         >>
            copyFile h1 h2

