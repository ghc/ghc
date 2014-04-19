-- !!! Testing RW handles 

import System.IO
import System.IO.Error
import System.Directory (removeFile, doesFileExist)
import Control.Monad
import System.Cmd

-- This test is weird, full marks to whoever dreamt it up!

main :: IO ()
main = do
   let username = "readwrite002.inout"
   f <- doesFileExist username
   when f (removeFile username)
   cd <- openFile username ReadWriteMode

   -- binary mode needed, otherwise newline translation gives
   -- unpredictable results.
   hSetBinaryMode cd True

-- Leva buffering on to make things more interesting:
--   hSetBuffering stdin NoBuffering
--   hSetBuffering stdout NoBuffering
--   hSetBuffering cd NoBuffering
   hPutStr cd speakString
   hSeek cd AbsoluteSeek 0
   speak cd  `catchIOError` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else ioError err
   hSeek cd AbsoluteSeek 0
   hSetBuffering cd LineBuffering
   speak cd  `catchIOError` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else ioError err
   return ()
   hSeek cd AbsoluteSeek 0
   hSetBuffering cd (BlockBuffering Nothing)
   speak cd  `catchIOError` \ err -> if isEOFError err then putStrLn "\nCaught EOF" else ioError err

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
