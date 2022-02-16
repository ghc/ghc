{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Driver.Pipeline.Phases
where

type role TPhase nominal
data TPhase res

