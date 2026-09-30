import Control.Exception
import System.IO

main :: IO ()
main = do
  h <- openFile "/dev/full" WriteMode
  hPutStr h "x"
  r <- try (hClose h)
  case r of
    Left e -> putStrLn (displayExceptionWithInfo e)
    Right () -> putStrLn "no exception"
