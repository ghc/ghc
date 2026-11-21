{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleContexts, TypeFamilies #-}
module NativeRetainedFamilyDomain where

import ConstrainedElements (Collects(..), FieldKey)
import Data.Proxy (Proxy(..))

#ifdef WRONG_TAG
different :: foreach (a :: Bool) -> foreach (b :: Bool)
          -> Collects (FieldKey a) => Proxy '(a, Elem (FieldKey b))
different _ _ = Proxy
#elif defined(LATE_GIVEN)
earlier :: Proxy (Elem (FieldKey 'False))
        -> foreach (b :: Bool) -> Collects (FieldKey 'False) => Proxy b
earlier _ _ = Proxy
#else
missing :: foreach (tag :: Bool) -> Proxy (Elem (FieldKey tag))
missing _ = Proxy
#endif
