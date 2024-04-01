{-# LANGUAGE TemplateHaskell, TypeAbstractions #-}

module Te where

import Language.Haskell.TH


$(
  [d|
    f :: forall a. a -> a
    f @t x = x :: t
  |]
  )
