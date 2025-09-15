import GHC.Conc
import Control.Exception
import System.IO.Unsafe
import System.Timeout

main :: IO ()
main = do
    x <- unsafeInterleaveIO $ atomically retry
    _ <- timeout 500000 $ evaluate x
    evaluate x
