import Control.Concurrent
import Control.Monad
import Foreign.C
import GHC.Clock
import GHC.IO.Device
import GHC.IO.Handle.FD
import System.IO
import System.Posix.IO
import System.Posix.Types
import System.Timeout

main :: IO ()
main = do
    socketHandle <- makeTestSocketHandle
    let nanoSecondsPerSecond = 1000 * 1000 * 1000
    let milliSecondsPerSecond = 1000
    let timeToSpend = 1
    let timeToSpendNano = timeToSpend * nanoSecondsPerSecond
    let timeToSpendMilli = timeToSpend * milliSecondsPerSecond
    start <- getMonotonicTimeNSec
    b <- hWaitForInput socketHandle timeToSpendMilli
    end <- getMonotonicTimeNSec
    let timeSpentNano = fromIntegral $ end - start
    let delta = timeSpentNano - timeToSpendNano
    -- We can never wait for a shorter amount of time than specified
    putStrLn $ "delta >= 0: " ++ show (delta >= 0)

foreign import ccall unsafe "socket" c_socket ::
               CInt -> CInt -> CInt -> IO CInt

makeTestSocketHandle :: IO Handle
makeTestSocketHandle = do
    sockNum <-
        c_socket
            1 -- PF_LOCAL
            2 -- SOCK_DGRAM
            0
    let fd = fromIntegral sockNum :: Fd
    h <-
        fdToHandle'
            (fromIntegral fd)
            (Just GHC.IO.Device.Stream)
            True
            "testsocket"
            ReadMode
            True
    hSetBuffering h NoBuffering
    pure h
