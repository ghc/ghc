{-# LANGUAGE TemplateHaskell #-}
module T23405 (test) where

import Language.Haskell.TH

test :: IO ()
test = do
  let s = $(getDoc (DeclDoc ''Double) >>= \doc -> [|doc|])
  print (s `seq` ())


