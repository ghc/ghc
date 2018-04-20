{-# LANGUAGE TemplateHaskell #-}
module Panic where

import Language.Haskell.TH

expr :: IO Exp
expr = runQ $ do
  name <- newName "foo"
  [| do $(varP name) <- pure (); pure () |]
