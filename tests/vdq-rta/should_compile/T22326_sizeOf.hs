{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_sizeOf where

import Foreign.Storable

-- Definition:
sizeOfVis :: forall a -> Storable a => Int
sizeOfVis (type t) = sizeOf (undefined :: t)

-- Usage:
sInt    = sizeOfVis (type Int)
sDouble = sizeOfVis (type Double)
sBool   = sizeOfVis (type Bool)
sChar   = sizeOfVis (type Char)
sUnit   = sizeOfVis (type ())