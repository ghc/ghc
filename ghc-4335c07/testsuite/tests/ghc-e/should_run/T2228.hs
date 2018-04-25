import System.IO
main = do
  hGetBuffering stdin >>= print
  hGetBuffering stdout >>= print
