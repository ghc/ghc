--!!! test various I/O Requests
--
--
import IO
import System
import IOBase (trace)
--import Trace ToDo: get this via GlaExts -- SOF

main = do
    prog <- getProgName
    hPutStr stderr (shows prog "\n")
    args <- getArgs
    hPutStr stderr (shows args "\n")
    path <- getEnv "PATH"
    hPutStr stderr (shows path "\n")
    stdin_txt <- getContents
    putStr stdin_txt
    file_cts <- readFile (head args)
    hPutStr  stderr file_cts
    trace "hello, trace" $
      catch (getEnv "__WURBLE__" >> return ()) (\ e -> error "hello, error\n")
