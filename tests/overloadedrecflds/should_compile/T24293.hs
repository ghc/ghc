{-# LANGUAGE TemplateHaskell #-}
module T24293 where
import Language.Haskell.TH

data Cheval = Cheval { hibou :: Int }

name = $(do
  n <- lookupValueName "hibou"
  pure $ LitE $ StringL $ show n)
