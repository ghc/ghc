{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module WrongPayload where

import GHC.Exists
import Provider

bad :: Hidden
bad = Pack (type Bool) (Just 'a')
