-- Test for #3128, file descriptor leak when hClose fails

import System.IO
import Control.Exception
import Data.Char

import System.Posix
import qualified GHC.IO.Device as IODevice
import GHC.IO.Handle
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import System.Posix.Internals

main = do
  (read,write) <- createPipe
  hread <- fdToHandle read
  hwrite <- fdToHandle write

        -- close the FD without telling the IO library:
  showPossibleException (hClose hread)
  hIsOpen hread >>= print

        -- put some data in the Handle's write buffer:
  hPutStr hwrite "testing"
        -- now try to close the Handle:
  showPossibleException (hClose hwrite)
  hIsOpen hwrite >>= print

showPossibleException :: IO () -> IO ()
showPossibleException f = do 
  e <- try f
  putStrLn (sanitise (show (e :: Either SomeException ())))
 where
  -- we don't care which file descriptor it is
  sanitise [] = []
  sanitise (x:xs) = if isDigit x then ('X':(sanitise' xs)) else (x:(sanitise xs))
  sanitise' [] = []
  sanitise' (x:xs) = if isDigit x then (sanitise' xs) else (x:(sanitise xs))

naughtyClose h = 
  withHandle_ "naughtyClose" h $ \ Handle__{haDevice=dev} -> do
     IODevice.close dev
