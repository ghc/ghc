import GHC.IO
import GHC.IO.Handle
import GHC.IO.Handle.Types
import System.IO
import Control.Concurrent.MVar
import Data.Typeable
import qualified GHC.IO.FD as FD

main = do
   h <- openFile "tmp" WriteMode
   hDuplicateTo h stdout
   
   fdh <- getfd h
   fdstdout <- getfd stdout
   hPutStrLn stderr ("h: " ++ show (fdh /= fdstdout) ++ "\nstdout: " ++ show fdstdout)

   hClose h
   putStrLn "bla"


getfd h@(FileHandle _ mvar) = do
  withMVar mvar $ \h__@Handle__{haDevice=dev} ->
   case cast dev of
     Just fd -> return (FD.fdFD fd)
     Nothing -> error "getfd"
