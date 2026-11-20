{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module WrongWitnessKind where

import GHC.Exists
import Provider

bad :: Hidden
bad = Pack (type 'True) Nothing
