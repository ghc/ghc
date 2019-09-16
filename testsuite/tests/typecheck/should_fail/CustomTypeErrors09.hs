{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module CustomTypeErrors09 where

import GHC.TypeLits


type family Deferred (e :: ErrorMessage) where
  Deferred e = TypeErrorPriority 'LowPriority e


bad :: Deferred ('Text "Error") => Int
bad = 5

problem :: Int
problem = bad

