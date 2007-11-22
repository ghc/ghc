import System.IO
import Control.Exception

import GHC.IOBase
import GHC.Handle
import System.Posix.Internals

main = do
  h <- openFile "hClose002.tmp" WriteMode
        -- close the FD without telling the IO library:
  naughtyClose h
        -- first hClose will raise an exception, but close the
        -- Handle anyway:
  try (hClose h) >>= print
        -- second hClose should success (Handle is already closed)
  try (hClose h) >>= print
        -- this should succeed (checking that the lock on the file has
        -- been released:
  h <- openFile "hClose002.tmp" ReadMode
  try (hClose h) >>= print
  try (hClose h) >>= print

naughtyClose h = 
  withHandle_ "naughtyClose" h $ \ h_ -> do
     let fd = haFD h_
     c_close fd
