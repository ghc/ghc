-- !!! hIsEOF (on stdout)

import System.IO ( hIsEOF, stdout )
import System.IO.Error

main = do
  flg <- hIsEOF stdout `catchIOError` \ _ -> putStrLn "hIsEOF failed" >> return False
  print flg
