import GHC.Handle
import GHC.IOBase
import GHC.Conc
import IO

main = do
   h <- openFile "tmp" WriteMode
   hDuplicateTo h stdout
   
   fdh <- getfd h
   fdstdout <- getfd stdout
   hPutStrLn stderr ("h: " ++ show fdh ++ "\nstdout: " ++ show fdstdout)

   hClose h
   putStrLn "bla"


getfd h@(FileHandle _ mvar) = do
   h__ <- takeMVar mvar
   let fd = fromIntegral (haFD h__)
   putMVar mvar h__
   return fd
