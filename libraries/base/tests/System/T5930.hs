import Control.Monad (when)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    term <- lookupEnv "PATH"
    when (isJust term) $ putStrLn "Got PATH"
    fish <- lookupEnv "One fish, two fish, red fish, blue fish"
    print fish
