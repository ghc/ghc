import System.Environment (getProgName)
import System.Process (getCurrentPid)

main :: IO ()
main = do
  prog <- getProgName
  pid <- getCurrentPid
  -- create conflicts with the .tix file that'd normally get created to check
  -- that filename generation picks something new and unique instead
  writeFile ("tix-files/" ++ prog ++ "-" ++ show pid ++ "-0.tix") "foo"
