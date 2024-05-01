{-# LANGUAGE PatternSynonyms #-}
module Test24749 where

-- c0
pattern (:|) ::
  -- c1
  a ->
  -- c2
  a ->
  -- c3
  Domino a
