{-# LANGUAGE TemplateHaskell #-}

module T25473B where

import Language.Haskell.TH
import T25473A

$(runIO $ do
  _ <- mkJSBinOp (+)
  pure [])
