module Main where

import TysWiredIn
import UniqSet
import Unique

import System.IO
import Control.Monad

main :: IO ()
main = sequence_
    [ uniq_tests ]


uniq_tests :: IO ()
uniq_tests = do
    let tycons   = map sumTyCon [2 .. 20]
        datacons = [ sumDataCon alt arity | arity <- [ 2 .. 20 ]
                                          , alt   <- [ 1 .. arity ] ]

        us = mkUniqSet (map getUnique tycons)
               `unionUniqSets` mkUniqSet (map getUnique datacons)

    when (sizeUniqSet us /= length tycons + length datacons) $ do
      hPutStrLn stderr "Sum cons/tycons have same uniques."
      hFlush stderr
