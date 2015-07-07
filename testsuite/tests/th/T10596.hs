{-# LANGUAGE TemplateHaskell #-}
module T10596 where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
do
  putQ (100 :: Int)
  x <- (getQ :: Q (Maybe Int))

  -- It should print "Just 100"
  runIO $ print x
  return []
