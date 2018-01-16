-- !!! Testing EOF (and the clearing of it)

module Main(main) where

import System.IO
import System.Directory ( removeFile )

main :: IO ()
main = do
   hdl <- openFile "hSeek002.hs" ReadMode
   flg <- hIsEOF hdl
   print flg
   hSeek hdl SeekFromEnd 0
   flg <- hIsEOF hdl
   print flg
   hSeek hdl SeekFromEnd (-1)
   flg <- hIsEOF hdl
   print flg
   hGetChar hdl
   flg <- hIsEOF hdl
   print flg
   hSeek hdl SeekFromEnd (-1)
   flg <- hIsEOF hdl
   print flg
   hClose hdl
