{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module WrongSchema where

import GHC.Exists

bad = Pack @Bool (type Bool) (Just True)
