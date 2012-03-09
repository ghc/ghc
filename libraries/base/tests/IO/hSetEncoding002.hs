-- test for #4066

import System.IO

import GHC.IO.FD as FD (stdout)
import GHC.IO.Handle.FD as FD (fdToHandle)
import GHC.IO.Handle ( mkDuplexHandle )

main = do
  h <- mkDuplexHandle FD.stdout "stdout" Nothing noNewlineTranslation
  hSetEncoding h utf8
  hPutStrLn h "ö"
  hClose h
