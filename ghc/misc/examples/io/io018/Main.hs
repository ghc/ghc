import IO -- 1.3

import System(getArgs)

main =   getArgs                            >>=        \ [user,host] ->
         let username = (user ++ "@" ++ host) in
         openFile username ReadWriteMode    >>=        \ cd          ->
         hSetBuffering stdin NoBuffering    >>
         hSetBuffering stdout NoBuffering   >>
         hSetBuffering cd NoBuffering       >>
         hPutStr cd speakString             >>
         speak cd

speakString = "Someone wants to speak with you\n"

speak cd =
         (hReady cd                         >>=        \ ready       ->
         if ready then (hGetChar cd >>= putChar)
         else return ()                     >>

         hReady stdin                       >>=        \ ready       ->
         if ready then (getChar >>= hPutChar cd)
         else return ())                    >>

         speak cd
