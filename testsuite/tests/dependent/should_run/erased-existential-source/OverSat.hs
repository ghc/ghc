{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module OverSat where

import GHC.Exists
import Provider

bad = (Pack @Hidden Bool (Just True)) ()
