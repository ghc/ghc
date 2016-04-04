-- A variant of the T10296a.hs test case in which
--   - the FFI machinery has been eliminated
--   - a primop (deRefStablePtr#) is used to dereference the stable pointer
--   - the stable pointers are explicitly freed at the end


import Control.Concurrent
import Control.Monad
import Foreign.StablePtr


main :: IO ()
main = do
    sp <- newStablePtr ()
    _ <- forkIO $ forever $ deRefStablePtr sp >> threadDelay 0
    sps <- replicateM 1048576 $ newStablePtr ()
    ----------------------------------------------------------
    mapM_ freeStablePtr sps
    freeStablePtr sp
