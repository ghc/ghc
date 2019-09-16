{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module CustomTypeErrors08 where

import GHC.TypeLits


type family Deferred (e :: ErrorMessage) where
  Deferred e = TypeErrorPriority 'HighPriority e


bad :: Deferred ('Text "Error") => Int
bad = 5

problem :: ()
problem = bad

