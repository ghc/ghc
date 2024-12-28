import Data.Maybe
import GHC.Internal.ExecutionStack

main :: IO ()
main = do
  putStrLn =<< (fromJust <$> showStackTrace)
