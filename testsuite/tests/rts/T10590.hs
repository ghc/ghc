import Foreign.C
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Concurrent

-- The test works only on UNIX like.
-- unportable bits:
import qualified System.Posix.Internals as SPI
import qualified System.Posix.Types as SPT

pipe :: IO (CInt, CInt)
pipe = allocaArray 2 $ \fds -> do
    throwErrnoIfMinus1_ "pipe" $ SPI.c_pipe fds
    rd <- peekElemOff fds 0
    wr <- peekElemOff fds 1
    return (rd, wr)

main :: IO ()
main = do
    (r1, w1)  <- pipe
    (r2, _w2) <- pipe
    _ <- forkIO $ do -- thread A
                     threadWaitRead (SPT.Fd r1)
    _ <- forkIO $ do -- thread B
                     threadWaitRead (SPT.Fd r2)
    yield -- switch to A, then B
          -- now both are blocked
    _ <- SPI.c_close w1 -- unblocking thread A fd
    _ <- SPI.c_close r2 -- breaking   thread B fd
    yield -- kick RTS IO manager

{-
 #10590 exposed a bug as:
   T10590: internal error: removeThreadFromDeQueue: not found
    (GHC version 7.11.20150702 for x86_64_unknown_linux)
    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
 -}
