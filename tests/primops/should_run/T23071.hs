import Control.Monad
import GHC.Conc.Sync

main = replicateM_ 1000000 $ listThreads >>= print

