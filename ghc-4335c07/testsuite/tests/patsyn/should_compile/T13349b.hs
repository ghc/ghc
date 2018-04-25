{-# LANGUAGE PatternSynonyms #-}

module T13349b where

pattern Nada = Nothing

-- Not orphan because it mentions the locally-defined Nada.
{-# COMPLETE Just, Nada #-}
