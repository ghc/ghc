{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
module T24293b where
import Language.Haskell.TH

data Cheval = Cheval { hibou :: Int }

hibou :: Bool
hibou = False

name = $(do
  n <- lookupValueName "hibou"
  pure $ LitE $ StringL $ show n)
