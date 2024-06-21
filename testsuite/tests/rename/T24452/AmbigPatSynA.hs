-- A module that is ambiguous with AmbigPatSynB
{-# LANGUAGE PatternSynonyms #-}
module AmbigPatSynA where
  pattern MkT{foo} = foo