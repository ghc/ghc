{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module CheckedEitherInput (Input) where

import qualified Prelude as P

type family Input (checked :: P.Bool) :: P.Either P.Bool (P.Maybe P.Bool) where
    Input 'P.True = 'P.Right (P.fmap P.not ('P.Just 'P.True))
    Input 'P.False = 'P.Right ('P.Just 'P.False)
