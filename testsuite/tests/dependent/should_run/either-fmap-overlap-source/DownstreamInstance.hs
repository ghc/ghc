{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module DownstreamInstance () where

import Data.Kind (Type)
import ResultFunctor

#ifdef INCOHERENT_CASE
instance {-# INCOHERENT #-} Translate (Result Type) where
#else
instance {-# OVERLAPPING #-} Translate (Result Type) where
#endif
    translate _ (Failed e) = Failed e
    translate f (Succeeded x) = Failed (f x)
    count _ = 2
