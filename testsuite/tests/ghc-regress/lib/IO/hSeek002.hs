-- !!! Testing EOF (and the clearing of it)

module Main(main) where

import IO
import Directory ( removeFile )
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main :: IO ()
main = do
   hdl <- openFile "hSeek002.hs" ReadMode
#ifdef i386_unknown_mingw32
   hSetBinaryMode hdl True
#endif
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
