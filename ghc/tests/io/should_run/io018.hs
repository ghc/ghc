--!!! Testing RW handles 
module Main(main) where


import IO
import Directory (removeFile)

-- This test is weird, full marks to whoever dreamt it up!

main :: IO ()
main = do
   let username = "io018.inout"
   cd <- openFile username ReadWriteMode
   removeFile username
   hSetBuffering stdin NoBuffering
   hSetBuffering stdout NoBuffering
   hSetBuffering cd NoBuffering
   hPutStr cd speakString
   hSeek cd AbsoluteSeek 0
   speak cd  `catch` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else fail err
   hSeek cd AbsoluteSeek 0
   hSetBuffering cd LineBuffering
   speak cd  `catch` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else fail err
   hSeek cd AbsoluteSeek 0
   hSetBuffering cd (BlockBuffering Nothing)
   speak cd  `catch` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else fail err

speakString = "Someone wants to speak with you\n"

speak cd = do
     (do
        ready <- hReady cd
        if ready then 
	   hGetChar cd >>= putChar
	 else
	   return ()
        ready <- hReady stdin
        if ready then (do { ch <- getChar; hPutChar cd ch})
         else return ())
     speak cd

