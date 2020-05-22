{-# LANGUAGE TemplateHaskell #-}

module THPutDocNonExistent where

import Language.Haskell.TH

class A a where
data B

do
  t <- [t| A B |]
  putDoc (InstDoc t) "a"
  pure []
