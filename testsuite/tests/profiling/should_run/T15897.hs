import Control.Monad
import Data.IORef
import System.Environment

main = do
    [n] <- getArgs
    replicateM (read n) (newIORef [1,2,3])
