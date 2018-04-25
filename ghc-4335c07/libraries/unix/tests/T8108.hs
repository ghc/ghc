import Control.Monad
import Control.Concurrent
import System.Posix.User

main = do
    void $ forkIO $ forever $ getGroupEntryForID 0
    void $ forkIO $ forever $ getGroupEntryForID 0
    threadDelay (3*1000*1000)
