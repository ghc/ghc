import IO -- 1.3

import System (getArgs)
import Char   (toUpper)

main   =  getArgs                           >>=        \ [f1,f2] ->
          openFile f1 ReadMode              >>=        \ h1      ->
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

