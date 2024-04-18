{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC "-Wunused-do-bind" #-}

module Example where

import T8671adefs qualified as Q

example :: Q.NotMonad 2
example = Q.do
  Q.incr
  Q.incr
