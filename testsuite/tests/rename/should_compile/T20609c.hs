{-# LANGUAGE PatternSynonyms #-}

module T20609c where

pattern Pat forall = forall

pattern RPat { forall } = forall

