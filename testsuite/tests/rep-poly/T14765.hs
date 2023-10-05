{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MagicHash, PolyKinds, ExplicitForAll #-}

module T14765 where

import GHC.Exts

fold :: forall rep a (r :: TYPE rep).
          (r -> a -> Proxy# r -> r) -> (Proxy# r -> r) -> [a] -> r
fold f k [] = k proxy#
fold f k (x : xs) = fold f (f (k proxy#) x) xs
