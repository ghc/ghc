{-# LANGUAGE TemplateHaskell #-}
module T24572c where

import Language.Haskell.TH


$(pure
  [ DataD [] (mkName "D") [] Nothing [NormalC (mkName "->") []] []])
