-- !!! hIsEOF (on stdout)

import IO ( hIsEOF, stdout )

main = do
  flg <- hIsEOF stdout `catch` \ _ -> putStrLn "hIsEOF failed" >> return False
  print flg
