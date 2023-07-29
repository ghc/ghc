{-# LANGUAGE PatternSynonyms #-}

module T20609c where

-- Declarations in this module used to be accepted by GHC
-- before `forall` became a keyword (#23719).

pattern Pat forall = forall

pattern RPat { forall } = forall

