
{-# LANGUAGE PatternSynonyms #-}

module T15681_PatSyn where

import Data.Functor.Identity

pattern U :: ()
pattern U = ()

foo :: Identity Char
foo = do
  U <- return ()
  return 'c'

{-# COMPLETE U #-}