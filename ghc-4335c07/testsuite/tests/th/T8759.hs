{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}

module T8759 where

import Language.Haskell.TH

pattern P = ()

$( do info <- reify 'P
      reportWarning (show info)
      return [] )
