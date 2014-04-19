-- !!! Reconfiguring the buffering of a handle
module Main(main) where

import System.IO
import System.IO.Error

queryBuffering :: String -> Handle -> IO ()
queryBuffering handle_nm handle = do
  bufm  <- hGetBuffering handle
  putStrLn ("Buffering for " ++ handle_nm ++ " is: " ++ show bufm)
   
main = do
  queryBuffering "stdin" stdin
  queryBuffering "stdout" stdout
  queryBuffering "stderr" stderr

   -- twiddling the setting for stdin.
  hSetBuffering stdin NoBuffering
  queryBuffering "stdin" stdin
  hSetBuffering stdin LineBuffering
  queryBuffering "stdin" stdin
  hSetBuffering stdin (BlockBuffering (Just 2))
  queryBuffering "stdin" stdin
  hSetBuffering stdin (BlockBuffering Nothing)
  queryBuffering "stdin" stdin
  let bmo = BlockBuffering (Just (-3))
  hSetBuffering stdin bmo `catchIOError` \ _ -> putStrLn ("Caught illegal op: hSetBuffering stdin " ++ showParen True (showsPrec 9 bmo) [])

  putChar '\n'

   -- twiddling the buffering for stdout
  hPutStr stdout "Hello stdout 1"
  hSetBuffering stdout NoBuffering
  queryBuffering "stdout" stdout
  hPutStr stdout "Hello stdout 2"
  hSetBuffering stdout LineBuffering
  queryBuffering "stdout" stdout
  hPutStr stdout "Hello stdout 3"
  hSetBuffering stdout (BlockBuffering (Just 2))
  queryBuffering "stdout" stdout
  hPutStr stdout "Hello stdout 4"
  hSetBuffering stdout (BlockBuffering Nothing)
  queryBuffering "stdout" stdout
  hPutStr stdout "Hello stdout 5"
  let bmo = BlockBuffering (Just (-3))
  hSetBuffering stdout bmo `catchIOError` \ _ -> putStrLn ("Caught illegal op: hSetBuffering stdout " ++ showParen True (showsPrec 9 bmo) [])

  putChar '\n'

   -- twiddling the buffering for stderr
  hPutStr stderr "Hello stderr 1"
  hSetBuffering stderr NoBuffering
  queryBuffering "stderr" stderr
  hPutStr stderr "Hello stderr 2"
  hSetBuffering stderr LineBuffering
  queryBuffering "stderr" stderr
  hPutStr stderr "Hello stderr 3"
  hSetBuffering stderr (BlockBuffering (Just 2))
  queryBuffering "stderr" stderr
  hPutStr stderr "Hello stderr 4"
  hSetBuffering stderr (BlockBuffering Nothing)
  queryBuffering "stderr" stderr
  hPutStr stderr "Hello stderr 5"
  let bmo = BlockBuffering (Just (-3))
  hSetBuffering stderr bmo `catchIOError` \ _ -> putStrLn ("Caught illegal op: hSetBuffering stderr " ++ showParen True (showsPrec 9 bmo) [])

  ls  <- hGetContents stdin
  ls' <- putLine ls
  hSetBuffering stdin NoBuffering
  putLine ls'
  return ()

putLine :: String -> IO String
putLine [] = return []
putLine (x:xs) = do
   putChar x
   case x of
     '\n' -> return xs
     _    -> putLine xs
  
