{-# LANGUAGE TemplateHaskell #-}
module T24572b where

import Language.Haskell.TH


$(pure
  [ DataD [] (mkName "D") [] Nothing [NormalC (mkName "~") []] []])
