-- !!! Testing RW handles 

import IO
import Directory (removeFile, doesFileExist)
import Monad
#ifdef mingw32_HOST_OS
import GHC.Handle(hSetBinaryMode)
#endif

-- This test is weird, full marks to whoever dreamt it up!

main :: IO ()
main = do
   let username = "readwrite002.inout"
   f <- doesFileExist username
   when f (removeFile username)
   cd <- openFile username ReadWriteMode
#ifdef mingw32_HOST_OS
   hSetBinaryMode cd True
#endif
   hSetBuffering stdin NoBuffering
   hSetBuffering stdout NoBuffering
   hSetBuffering cd NoBuffering
   hPutStr cd speakString
   hSeek cd AbsoluteSeek 0
   speak cd  `catch` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else ioError err
   hSeek cd AbsoluteSeek 0
   hSetBuffering cd LineBuffering
   speak cd  `catch` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else ioError err
   hSeek cd AbsoluteSeek 0
   hSetBuffering cd (BlockBuffering Nothing)
   speak cd  `catch` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else ioError err

speakString = "##############################\n"

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
