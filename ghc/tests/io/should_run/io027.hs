-- !!! hIsEOF (on stdout)
module Main(main) where

import IO ( hIsEOF, stdout )

main = do
  flg <- hIsEOF stdout `catch` \ _ -> putStrLn "hIsEOF failed" >> return False
  print flg
