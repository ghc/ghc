{-# LANGUAGE ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module Client (zipLaw) where

import qualified HaskLib as H

-- | Use the public functions and equality in a new dependent signature.
zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> H.size (zip xs ys) H.:~: H.minSize (H.size xs) (H.size ys)
zipLaw xs ys = H.lengthZip xs ys
