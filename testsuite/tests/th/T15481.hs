{-# LANGUAGE TemplateHaskell #-}
module Bug where

import Language.Haskell.TH

main :: IO ()
main = putStrLn $(recover (stringE "reifyFixity failed")
                          (do foo <- newName "foo"
                              _ <- reifyFixity foo
                              stringE "reifyFixity successful"))
