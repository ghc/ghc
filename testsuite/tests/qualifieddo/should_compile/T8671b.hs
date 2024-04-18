{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC "-Wunused-do-bind" #-}

module Example where

import T8671bdefs qualified as Q

example :: Q.NotMonad Bool
example = Q.do
  Q.bool 1 2
  Q.bool 3 4
