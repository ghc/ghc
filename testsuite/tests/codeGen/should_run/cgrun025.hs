{-# LANGUAGE ScopedTypeVariables #-}
-- !!! test various I/O Requests
--
--
import Control.Exception
import System.Environment
import System.IO
import Debug.Trace (trace)
import Data.Maybe
import Data.List (isInfixOf)

main = do
    prog <- getProgName
    let True = "cgrun025" `isInfixOf` prog
    hPutStr stderr (shows prog "\n")
    args <- getArgs
    hPutStr stderr (shows args "\n")
    path <- getEnv "PATH"
    hPutStr stderr ("GOT PATH\n")
    stdin_txt <- getContents
    putStr stdin_txt
    file_cts <- readFile (head args)
    hPutStr  stderr file_cts
    trace "hello, trace" $
      catch (getEnv "__WURBLE__" >> return ()) (\ (e :: SomeExceptionWithLocation) -> error "hello, error")
