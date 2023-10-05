import Data.List (group)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  _ <- getContents
  if last (group "a") == "a" then exitSuccess else exitFailure

