-- Sigbjorn and I don't understand what this test is meant to do
-- It simply hangs on stdin!

import IO -- 1.3
import Directory (removeFile)

main =   let username = "io018.inout"	    in
         openFile username ReadWriteMode    >>=        \ cd          ->
         removeFile username		    >>
         hSetBuffering stdin NoBuffering    >>
         hSetBuffering stdout NoBuffering   >>
         hSetBuffering cd NoBuffering       >>
         hPutStr cd speakString             >>
         speak cd

speakString = "Someone wants to speak with you\n"

speak cd = return ()
{-
         (hReady cd                         >>=        \ ready       ->
         if ready then (hGetChar cd >>= putChar)
         else return ()                     >>

         hReady stdin                       >>=        \ ready       ->
         if ready then (getChar >>= hPutChar cd)
         else return ())                    >>

         speak cd
-}
