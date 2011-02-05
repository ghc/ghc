-- !!! hIsEOF (on stdout)

import System.IO ( hIsEOF, stdout )

main = do
  flg <- hIsEOF stdout `catch` \ _ -> putStrLn "hIsEOF failed" >> return False
  print flg
