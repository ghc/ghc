{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Foldable
import GHC.SysTools.Ar

main :: IO ()
main = for_ [writeBSDAr, writeGNUAr] $ \writer -> do
  writer "test.a" $
    Archive
      [ ArchiveEntry
          { filename = "1",
            filetime = 0,
            fileown = 0,
            filegrp = 0,
            filemode = 644,
            filesize = 1,
            filedata = "\NUL"
          }
      ]
  print =<< loadAr "test.a"
