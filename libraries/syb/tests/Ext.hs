{-# OPTIONS -fglasgow-exts #-}

module Ext () where

-- There were typos in these definitions in the ICFP 2004 paper.

import Data.Generics

extQ fn spec_fn arg
  = case gcast (Q spec_fn) of
      Just (Q spec_fn') -> spec_fn' arg
      Nothing           -> fn       arg
                                                                                
newtype Q r a = Q (a -> r)
                                                                                
extT fn spec_fn arg
  = case gcast (T spec_fn) of
      Just (T spec_fn') -> spec_fn' arg
      Nothing           -> fn       arg
                                                                                
newtype T a = T (a -> a)

extM :: (Typeable a, Typeable b)
     => (a -> m a) -> (b -> m b) -> (a -> m a)
extM fn spec_fn
  = case gcast (M spec_fn) of
      Just (M spec_fn') -> spec_fn'
      Nothing           -> fn

newtype M m a = M (a -> m a)
