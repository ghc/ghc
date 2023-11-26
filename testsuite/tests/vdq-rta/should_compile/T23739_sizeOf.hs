{-# LANGUAGE RequiredTypeArguments #-}

module T23739_sizeOf where

import Foreign.Storable

sizeOfVis :: forall a -> Storable a => Int
sizeOfVis t = sizeOf (undefined :: t)