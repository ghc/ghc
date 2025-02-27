{-# LANGUAGE TemplateHaskell #-}
module RnPatternSynonymFail where

import Language.Haskell.TH

$(pure [ PatSynD (mkName "None") (PrefixPatSyn []) ImplBidir (ConP 'Nothing [])])
