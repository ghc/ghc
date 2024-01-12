{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module T24293c where
import Language.Haskell.TH

data Cheval = Cheval { hibou :: Int }
data Agneau = Agneau { hibou :: Bool }

name = $(do
  n <- lookupValueName "hibou"
  pure $ LitE $ StringL $ show n)
