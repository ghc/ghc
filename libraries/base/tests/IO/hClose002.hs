import System.IO
import Control.Exception

import qualified GHC.IO.Device as IODevice
import GHC.IO.Handle
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import System.Posix.Internals

main = do
  h <- openFile "hClose002.tmp" WriteMode
        -- close the FD without telling the IO library:
  naughtyClose h
        -- first hClose will raise an exception, but close the
        -- Handle anyway:
  showPossibleException (hClose h)
        -- second hClose should success (Handle is already closed)
  showPossibleException (hClose h)
        -- this should succeed (checking that the lock on the file has
        -- been released:
  h <- openFile "hClose002.tmp" ReadMode
  showPossibleException (hClose h)
  showPossibleException (hClose h)

showPossibleException :: IO () -> IO ()
showPossibleException f = do e <- try f
                             print (e :: Either SomeException ())

naughtyClose h = 
  withHandle_ "naughtyClose" h $ \ Handle__{haDevice=dev} -> do
     IODevice.close dev

