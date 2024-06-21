-- A module that is ambiguous with AmbigPatSynA
{-# LANGUAGE PatternSynonyms #-}
module AmbigPatSynB where
  pattern MkT{foo} = foo