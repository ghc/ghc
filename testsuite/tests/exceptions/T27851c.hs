import Control.Exception
import System.IO

main :: IO ()
main = do
  withBinaryFile "T27851c.txt" WriteMode $ \h -> hPutStr h "\xff\n"
  run "hGetLine" hGetLine
  run "hGetContents'" hGetContents'
  run "hGetContents" $ \h -> do
    s <- hGetContents h
    length s `seq` pure s

run :: String -> (Handle -> IO String) -> IO ()
run name act = do
  r <- try $ withFile "T27851c.txt" ReadMode $ \h -> do
    hSetEncoding h utf8
    act h
  case r of
    Left e -> do
      putStrLn ("-- " ++ name)
      putStrLn (displayExceptionWithInfo e)
    Right s -> print s
