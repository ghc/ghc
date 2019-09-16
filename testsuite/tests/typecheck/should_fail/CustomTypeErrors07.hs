{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module CustomTypeErrors07 where

import GHC.TypeLits


type family Deferred (e :: ErrorMessage) where
  Deferred e = TypeErrorPriority 'LowPriority e


bad :: Deferred ('Text "Error") => Int
bad = 5

problem :: ()
problem = bad

